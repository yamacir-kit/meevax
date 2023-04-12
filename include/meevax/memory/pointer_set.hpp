/*
   Copyright 2018-2023 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_MEMORY_POINTER_SET_HPP
#define INCLUDED_MEEVAX_MEMORY_POINTER_SET_HPP

#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <limits>
#include <type_traits>
#include <vector>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  constexpr auto log2(T x) noexcept -> T
  {
    return (x < 2) ? 1 : log2(x / 2) + 1;
  }

  static_assert(log2(0b0001) - 1 == 0);
  static_assert(log2(0b0010) - 1 == 1);
  static_assert(log2(0b0100) - 1 == 2);
  static_assert(log2(0b1000) - 1 == 3);

  template <typename Pointer, std::size_t word_size = 8 * 1024 * 16>
  class pointer_set
  {
    static_assert(std::is_pointer_v<Pointer>);

    struct compact_pointer
    {
      std::uintptr_t const value;

      static constexpr auto width = log2(alignof(std::remove_pointer_t<Pointer>)) - 1;

      constexpr compact_pointer(Pointer p)
        : value { reinterpret_cast<std::uintptr_t>(p) >> width }
      {}

      static constexpr auto restore(std::uintptr_t value)
      {
        return reinterpret_cast<Pointer>(value << width);
      }

      explicit constexpr operator Pointer() const noexcept
      {
        return reinterpret_cast<Pointer>(value << width);
      }
    };

    static constexpr auto page_number_of(compact_pointer p) noexcept
    {
      return p.value / word_size;
    }

    static constexpr auto word_number_of(compact_pointer p) noexcept
    {
      return p.value - page_number_of(p) * word_size;
    }

    struct page
    {
      std::size_t number;

      std::array<bool, word_size> word {};

      explicit constexpr page(std::size_t number)
        : number { number }
      {}

      constexpr auto operator [](std::size_t index) const noexcept -> decltype(auto)
      {
        return word[index];
      }

      constexpr auto operator [](std::size_t index) noexcept -> decltype(auto)
      {
        return word[index];
      }

      constexpr auto operator <(std::size_t other_page_number) noexcept
      {
        return number < other_page_number;
      }
    };

    std::vector<page> pages;

  public:
    struct iterator
    {
      using iterator_category = std::bidirectional_iterator_tag;

      using value_type = Pointer;

      using reference = std::add_lvalue_reference_t<value_type>;

      using pointer = std::add_pointer_t<value_type>;

      using difference_type = std::ptrdiff_t;

      page const* pages;

      std::size_t page_size;

      std::size_t page_index;

      std::size_t word_index;

      explicit iterator(std::vector<page> const& pages,
                        std::size_t page_index_hint,
                        std::size_t word_index_hint) noexcept
        : pages      { pages.data() }
        , page_size  { pages.size() }
        , page_index { page_index_hint }
        , word_index { word_index_hint }
      {
        if (not operator bool())
        {
          operator ++();
        }
      }

      explicit iterator(std::vector<page> const& pages) noexcept
        : pages      { pages.data() }
        , page_size  { pages.size() }
        , page_index { page_size }
        , word_index { word_size }
      {}

      explicit operator bool() const noexcept
      {
        return page_index < page_size and
               word_index < word_size and pages[page_index][word_index];
      }

      auto operator *() const noexcept
      {
        return compact_pointer::restore(pages[page_index].number * word_size + word_index);
      }

      auto operator ++() noexcept -> auto &
      {
        ++word_index;

        for (; page_index < page_size; ++page_index)
        {
          for (auto&& word = pages[page_index]; word_index < word_size; ++word_index)
          {
            if (word[word_index])
            {
              return *this;
            }
          }

          word_index = 0;
        }

        page_index = page_size;
        word_index = word_size;

        return *this; // end
      }

      auto operator ++(int) noexcept
      {
        auto copy = *this;
        operator ++();
        return copy;
      }

      auto operator --() noexcept -> auto &
      {
        page_index = page_size <= page_index ? page_size - 1 : page_index;
        word_index = word_size <= word_index ? word_size - 1 : word_index - 1;

        /*
           NOTE: N4659 6.9.1.4

           Unsigned integers shall obey the laws of arithmetic modulo 2 n where
           n is the number of bits in the value representation of that
           particular size of integer.
        */
        for (; page_index < page_size; --page_index)
        {
          for (auto&& word = pages[page_index]; word_index < word_size; --word_index)
          {
            if (word[word_index])
            {
              return *this;
            }
          }

          word_index = word_size - 1;
        }

        return *this;
      }

      auto operator --(int) noexcept
      {
        auto copy = *this;
        operator --();
        return copy;
      }

      auto operator ==(iterator const& rhs) noexcept
      {
        return page_index == rhs.page_index and word_index == rhs.word_index;
      }

      auto operator !=(iterator const& rhs) noexcept
      {
        return not operator ==(rhs);
      }
    };

    explicit pointer_set()
      : pages {}
    {
      pages.reserve(64);

      assert(pages.size() == 0);
      assert(begin() == end());
    }

    auto lower_bound_page_of(compact_pointer p) noexcept
    {
      return std::lower_bound(std::begin(pages), std::end(pages), page_number_of(p));
    }

    [[deprecated]]
    auto page_count() const noexcept
    {
      return pages.size();
    }

    auto size() const noexcept
    {
      return std::distance(begin(), end());
    }

    auto insert(Pointer p) noexcept
    {
      if (auto iter = lower_bound_page_of(p); iter != std::end(pages) and iter->number == page_number_of(p))
      {
        (*iter)[word_number_of(p)] = true;
      }
      else
      {
        assert(iter == std::end(pages) or page_number_of(p) < iter->number);
        iter = pages.emplace(iter, page_number_of(p));
        (*iter)[word_number_of(p)] = true;
      }
    }

    auto erase(Pointer p) noexcept
    {
      auto iter = lower_bound_page_of(p);

      assert(iter != std::end(pages));

      (*iter)[word_number_of(p)] = false;
    }

    [[deprecated]]
    auto erase(iterator iter) noexcept
    {
      erase(*iter);
      return ++iter;
    }

    auto begin() const noexcept
    {
      return iterator(pages, 0, 0);
    }

    auto end() const noexcept
    {
      return iterator(pages);
    }

    auto lower_bound(Pointer p) noexcept
    {
      if (auto iter = lower_bound_page_of(p); iter != std::end(pages))
      {
        return iterator(pages,
                        std::distance(std::begin(pages), iter),
                        word_number_of(p));
      }
      else
      {
        return end();
      }
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_POINTER_SET_HPP
