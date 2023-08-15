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

  template <typename Pointer, std::size_t Capacity = 1024 * 1024>
  class pointer_set
  {
    static_assert(std::is_pointer_v<Pointer>);

    static constexpr auto width = sizeof(std::uintmax_t) * 8;

    static_assert(Capacity % width == 0);

    struct compact_pointer
    {
      std::uintptr_t const value;

      static constexpr auto width = log2(alignof(std::remove_pointer_t<Pointer>)) - 1;

      constexpr compact_pointer(Pointer p)
        : value { reinterpret_cast<std::uintptr_t>(p) >> width }
      {}

      constexpr auto offset() const noexcept
      {
        return (value / Capacity) * Capacity;
      }

      constexpr auto index() const noexcept
      {
        return value - offset();
      }

      static constexpr auto to_pointer(std::uintptr_t value)
      {
        return reinterpret_cast<Pointer>(value << width);
      }
    };

    struct chunk
    {
      std::size_t offset;

      bool data[Capacity];

      explicit constexpr chunk(compact_pointer const p)
        : offset { p.offset() }
        , data { false }
      {
        data[p.index()] = true;
      }

      constexpr auto operator [](std::size_t index) const noexcept -> decltype(auto)
      {
        return data[index];
      }

      constexpr auto operator [](std::size_t index) noexcept -> decltype(auto)
      {
        return data[index];
      }

      constexpr auto operator <(compact_pointer p) noexcept
      {
        return offset < p.offset();
      }
    };

    std::vector<chunk> chunks;

  public:
    struct iterator
    {
      using iterator_category = std::bidirectional_iterator_tag;

      using value_type = Pointer;

      using reference = std::add_lvalue_reference_t<value_type>;

      using pointer = std::add_pointer_t<value_type>;

      using difference_type = std::ptrdiff_t;

      std::vector<chunk> const& chunks;

      std::size_t i, j;

      explicit iterator(std::vector<chunk> const& chunks,
                        std::size_t i,
                        std::size_t j) noexcept
        : chunks { chunks }
        , i      { i }
        , j      { j }
      {
        if (not (i < chunks.size() and j < Capacity and chunks[i][j]))
        {
          operator ++();
        }
      }

      explicit iterator(std::vector<chunk> const& chunks) noexcept
        : chunks { chunks }
        , i      { chunks.size() }
        , j      { Capacity }
      {}

      auto operator *() const noexcept
      {
        return compact_pointer::to_pointer(chunks[i].offset + j);
      }

      auto operator ++() noexcept -> auto &
      {
        ++j;

        for (; i < chunks.size(); ++i, j = 0)
        {
          for (; j < Capacity; ++j)
          {
            if (chunks[i][j])
            {
              return *this;
            }
          }
        }

        i = chunks.size();

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
        i = std::min(chunks.size() - 1, i);

        j = std::min(Capacity - 1, j - 1);

        /*
           NOTE: N4659 6.9.1.4

           Unsigned integers shall obey the laws of arithmetic modulo 2 n where
           n is the number of bits in the value representation of that
           particular size of integer.
        */
        for (; i < chunks.size(); --i, j = Capacity - 1)
        {
          for (; j < Capacity; --j)
          {
            if (chunks[i][j])
            {
              return *this;
            }
          }
        }

        return *this;
      }

      auto operator --(int) noexcept
      {
        auto copy = *this;
        operator --();
        return copy;
      }

      auto operator ==(iterator const& rhs) const noexcept
      {
        return i == rhs.i and j == rhs.j;
      }

      auto operator !=(iterator const& rhs) const noexcept
      {
        return not operator ==(rhs);
      }
    };

    explicit pointer_set()
      : chunks {}
    {
      assert(chunks.size() == 0);
      assert(begin() == end());
    }

    auto lower_bound_chunk(compact_pointer p) noexcept
    {
      return std::lower_bound(chunks.begin(), chunks.end(), p);
    }

    auto size() const noexcept
    {
      return std::distance(begin(), end());
    }

    auto insert(compact_pointer p) noexcept
    {
      if (auto iter = lower_bound_chunk(p); iter != chunks.end() and iter->offset == p.offset())
      {
        (*iter)[p.index()] = true;
      }
      else
      {
        assert(iter == chunks.end() or p.offset() < iter->offset);
        chunks.emplace(iter, p);
      }
    }

    auto erase(compact_pointer p) noexcept
    {
      auto iter = lower_bound_chunk(p);
      assert(iter != chunks.end());
      (*iter)[p.index()] = false;
    }

    auto begin() const noexcept
    {
      return iterator(chunks, 0, 0);
    }

    auto end() const noexcept
    {
      return iterator(chunks);
    }

    auto lower_bound(compact_pointer p) noexcept
    {
      if (auto iter = lower_bound_chunk(p); iter != chunks.end())
      {
        return iterator(chunks, std::distance(chunks.begin(), iter), p.index());
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
