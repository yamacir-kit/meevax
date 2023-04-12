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
#include <bitset>
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

  template <typename Pointer, std::size_t N = 1024 /* NOTE: This number has no technical basis and is probably not efficient */>
  class pointer_set
  {
    static_assert(std::is_pointer_v<Pointer>);

    static_assert(sizeof(std::uintptr_t) <= sizeof(std::uintmax_t));

    static constexpr auto char_index_max = std::numeric_limits<std::uintmax_t>::digits;

    static constexpr auto word_index_max = N;

    static constexpr auto page_number_of(std::uintmax_t value) noexcept
    {
      return value / (word_index_max * char_index_max);
    }

    static constexpr auto word_number_of(std::uintmax_t value) noexcept
    {
      return (value - page_number_of(value) * word_index_max * char_index_max) / char_index_max;
    }

    static constexpr auto char_number_of(std::uintmax_t value) noexcept
    {
      return value - page_number_of(value) * word_index_max * char_index_max
                   - word_number_of(value) * char_index_max;
    }

    static constexpr auto always_zero_digits_length = log2(alignof(std::remove_pointer_t<Pointer>)) - 1;

    static constexpr auto compress(Pointer p) noexcept -> std::uintmax_t
    {
      return reinterpret_cast<std::uintptr_t>(p) >> always_zero_digits_length;
    }

    static constexpr auto decompress(std::uintmax_t value) noexcept
    {
      return reinterpret_cast<Pointer>(value << always_zero_digits_length);
    }

    struct page
    {
      std::size_t number;

      std::array<std::bitset<char_index_max>, word_index_max> words;

      explicit constexpr page(std::size_t number)
        : number { number }
        , words {}
      {}

      constexpr auto operator [](std::size_t index) const noexcept -> decltype(auto)
      {
        return words[index];
      }

      constexpr auto operator [](std::size_t index) noexcept -> decltype(auto)
      {
        return words[index];
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

      std::size_t page_index_max;

      std::size_t page_index,
                  word_index,
                  char_index;

      explicit iterator(std::vector<page> const& pages,
                        std::size_t page_index_hint,
                        std::size_t word_index_hint,
                        std::size_t char_index_hint) noexcept
        : pages          { pages.data() }
        , page_index_max { pages.size() }
        , page_index     { page_index_hint }
        , word_index     { word_index_hint }
        , char_index     { char_index_hint }
      {
        if (not operator bool())
        {
          operator ++();
        }
      }

      explicit iterator(std::vector<page> const& pages) noexcept
        : pages          { pages.data() }
        , page_index_max { pages.size() }
        , page_index     { page_index_max }
        , word_index     { word_index_max }
        , char_index     { char_index_max }
      {}

      explicit operator bool() const noexcept
      {
        return page_index < page_index_max and
               word_index < word_index_max and
               char_index < char_index_max and pages[page_index][word_index][char_index];
      }

      auto operator *() const noexcept
      {
        return decompress(pages[page_index].number * word_index_max * char_index_max +
                          word_index * char_index_max +
                          char_index);
      }

      auto operator ++() noexcept -> auto &
      {
        ++char_index;

        for (; page_index < page_index_max; ++page_index)
        {
          for (auto&& words = pages[page_index]; word_index < word_index_max; ++word_index)
          {
            for (auto&& chars = words[word_index]; char_index < char_index_max; ++char_index)
            {
              if (chars[char_index])
              {
                return *this;
              }
            }

            char_index = 0;
          }

          word_index = 0;
        }

        page_index = page_index_max;
        word_index = word_index_max;
        char_index = char_index_max;

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
        page_index = page_index_max <= page_index ? page_index_max - 1 : page_index;
        word_index = word_index_max <= word_index ? word_index_max - 1 : word_index;
        char_index = char_index_max <= char_index ? char_index_max - 1 : char_index - 1;

        /*
           NOTE: N4659 6.9.1.4

           Unsigned integers shall obey the laws of arithmetic modulo 2 n where
           n is the number of bits in the value representation of that
           particular size of integer.
        */
        for (; page_index < page_index_max; --page_index)
        {
          for (auto&& words = pages[page_index]; word_index < word_index_max; --word_index)
          {
            for (auto&& chars = words[word_index]; char_index < char_index_max; --char_index)
            {
              if (chars[char_index])
              {
                return *this;
              }
            }

            char_index = char_index_max - 1;
          }

          word_index = word_index_max - 1;
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
        return page_index == rhs.page_index and
               word_index == rhs.word_index and
               char_index == rhs.char_index;
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

    auto lower_bound_page_of(std::uintmax_t value) noexcept
    {
      return std::lower_bound(std::begin(pages), std::end(pages), page_number_of(value));
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
      const auto id = compress(p);

      if (auto iter = lower_bound_page_of(id); iter != std::end(pages) and iter->number == page_number_of(id))
      {
        (*iter)[word_number_of(id)][char_number_of(id)] = true;
      }
      else
      {
        assert(iter == std::end(pages) or page_number_of(id) < iter->number);
        iter = pages.emplace(iter, page_number_of(id));
        (*iter)[word_number_of(id)][char_number_of(id)] = true;
      }
    }

    auto erase(Pointer p) noexcept
    {
      const auto id = compress(p);

      auto iter = lower_bound_page_of(id);

      assert(iter != std::end(pages));

      (*iter)[word_number_of(id)][char_number_of(id)] = false;
    }

    [[deprecated]]
    auto erase(iterator iter) noexcept
    {
      erase(*iter);
      return ++iter;
    }

    auto begin() const noexcept
    {
      return iterator(pages, 0, 0, 0);
    }

    auto end() const noexcept
    {
      return iterator(pages);
    }

    auto lower_bound(Pointer p) noexcept
    {
      auto id = compress(p);

      if (auto iter = lower_bound_page_of(id); iter != std::end(pages))
      {
        return iterator(pages,
                        std::distance(std::begin(pages), iter),
                        word_number_of(id),
                        char_number_of(id));
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
