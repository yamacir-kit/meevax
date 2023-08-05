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
#include <iostream>
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

  template <typename Pointer, std::size_t Capacity = 8 * 1024 * 16>
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

      constexpr auto page_number() noexcept
      {
        return value / Capacity;
      }

      constexpr auto index() noexcept
      {
        return value - page_number() * Capacity;
      }

      static constexpr auto to_pointer(std::uintptr_t value)
      {
        return reinterpret_cast<Pointer>(value << width);
      }

      explicit constexpr operator Pointer() const noexcept
      {
        return to_pointer(value);
      }
    };

    struct page
    {
      std::size_t number;

      bool data[Capacity];

      explicit constexpr page(std::size_t number)
        : number { number }
        , data { false }
      {}

      constexpr auto operator [](std::size_t index) const noexcept -> decltype(auto)
      {
        return data[index];
      }

      constexpr auto operator [](std::size_t index) noexcept -> decltype(auto)
      {
        return data[index];
      }

      constexpr auto operator <(std::size_t other_page_number) noexcept
      {
        return number < other_page_number;
      }

      friend auto operator <<(std::ostream & output, page const& page) -> std::ostream &
      {
        for (std::size_t i = 0; i < Capacity; )
        {
          output << reinterpret_cast<void *>(page.number * Capacity + i) << ":";

          for (auto j = i + 64; i < j; ++i)
          {
            output << (i % 8 ? "" : " ") << (page.data[i] ? "\x1b[31m1\x1b[0m" : "0");
          }

          output << std::endl;
        }

        return output;
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

      std::vector<page> const& pages;

      std::size_t i, j;

      explicit iterator(std::vector<page> const& pages,
                        std::size_t i,
                        std::size_t j) noexcept
        : pages { pages }
        , i     { i }
        , j     { j }
      {
        auto valid = [&]()
        {
          return i < pages.size() and j < Capacity and pages[i][j];
        };

        if (not valid())
        {
          operator ++();
        }
      }

      explicit iterator(std::vector<page> const& pages) noexcept
        : pages { pages }
        , i     { pages.size() }
        , j     { Capacity }
      {}

      auto operator *() const noexcept
      {
        return compact_pointer::to_pointer(pages[i].number * Capacity + j);
      }

      auto operator ++() noexcept -> auto &
      {
        ++j;

        for (; i < pages.size(); ++i)
        {
          for (; j < Capacity; ++j)
          {
            if (pages[i][j])
            {
              return *this;
            }
          }

          j = 0;
        }

        i = pages.size();

        j = Capacity;

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
        i = std::min(pages.size() - 1, i);

        j = std::min(Capacity - 1, j - 1);

        /*
           NOTE: N4659 6.9.1.4

           Unsigned integers shall obey the laws of arithmetic modulo 2 n where
           n is the number of bits in the value representation of that
           particular size of integer.
        */
        for (; i < pages.size(); --i)
        {
          for (; j < Capacity; --j)
          {
            if (pages[i][j])
            {
              return *this;
            }
          }

          j = Capacity - 1;
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
        return i == rhs.i and j == rhs.j;
      }

      auto operator !=(iterator const& rhs) noexcept
      {
        return not operator ==(rhs);
      }
    };

    explicit pointer_set()
      : pages {}
    {
      assert(pages.size() == 0);
      assert(begin() == end());
    }

    auto lower_bound_page_of(compact_pointer p) noexcept
    {
      return std::lower_bound(std::begin(pages), std::end(pages), p.page_number());
    }

    auto size() const noexcept
    {
      return std::distance(begin(), end());
    }

    auto insert(compact_pointer p) noexcept
    {
      if (auto iter = lower_bound_page_of(p); iter != std::end(pages) and iter->number == p.page_number())
      {
        (*iter)[p.index()] = true;
      }
      else
      {
        assert(iter == std::end(pages) or p.page_number() < iter->number);
        iter = pages.emplace(iter, p.page_number());
        (*iter)[p.index()] = true;
      }
    }

    auto erase(compact_pointer p) noexcept
    {
      auto iter = lower_bound_page_of(p);

      assert(iter != std::end(pages));

      (*iter)[p.index()] = false;
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

    auto lower_bound(compact_pointer p) noexcept
    {
      if (auto iter = lower_bound_page_of(p); iter != std::end(pages))
      {
        return iterator(pages, std::distance(std::begin(pages), iter), p.index());
      }
      else
      {
        return end();
      }
    }

    friend auto operator <<(std::ostream & output, pointer_set const& pointer_set) -> std::ostream &
    {
      output << "size = " << pointer_set.size() << std::endl;

      output << "density = " << (pointer_set.size() / double(pointer_set.pages.size() * Capacity)) << std::endl;

      for (auto && page : pointer_set.pages)
      {
        output << page << std::endl;
      }

      return output;
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_POINTER_SET_HPP
