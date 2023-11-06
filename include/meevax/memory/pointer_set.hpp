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

#include <meevax/bit/log2.hpp>
#include <meevax/bitset/simple_bitset.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename Pointer,
            template <std::size_t> typename Bitset = simple_bitset,
            std::size_t N = 1024 * 1024>
  class pointer_set
  {
    static_assert(std::is_pointer_v<Pointer>);

    static constexpr auto width = sizeof(std::uintptr_t) * 8;

    static_assert(N % width == 0);

    struct compact_pointer
    {
      std::uintptr_t const value;

      static constexpr auto width = log2(alignof(std::remove_pointer_t<Pointer>)) - 1;

      constexpr compact_pointer(Pointer p)
        : value { reinterpret_cast<std::uintptr_t>(p) >> width }
      {}

      constexpr auto offset() const noexcept
      {
        return (value / N) * N;
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

    struct chunk : public Bitset<N>
    {
      std::size_t offset;

      explicit chunk(compact_pointer const p) noexcept
        : offset { p.offset() }
      {
        Bitset<N>::set(p.index());
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

      std::size_t i;

      std::size_t j;

      explicit iterator(std::vector<chunk> const& chunks,
                        typename std::vector<chunk>::const_iterator iter,
                        std::size_t j) noexcept
        : chunks { chunks }
        , i      { static_cast<std::size_t>(std::distance(chunks.begin(), iter)) }
        , j      { j }
      {
        if (not (i < chunks.size() and j < N and chunks[i].test(j)))
        {
          operator ++();
        }
      }

      explicit iterator(std::vector<chunk> const& chunks) noexcept
        : chunks { chunks }
        , i      { chunks.size() }
        , j      { N }
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
          for (; j < N; ++j)
          {
            if (chunks[i].test(j))
            {
              return *this;
            }
          }
        }

        i = chunks.size();

        j = N;

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

        j = std::min(N - 1, j - 1);

        /*
           NOTE: N4659 6.9.1.4

           Unsigned integers shall obey the laws of arithmetic modulo 2 n where
           n is the number of bits in the value representation of that
           particular size of integer.
        */
        for (; i < chunks.size(); --i, j = N - 1)
        {
          for (; j < N; --j)
          {
            if (chunks[i].test(j))
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
      auto compare = [](auto && chunk, auto && offset) constexpr
      {
        return chunk.offset < offset;
      };

      return std::lower_bound(chunks.begin(), chunks.end(), p.offset(), compare);
    }

    auto size() const noexcept
    {
      return std::distance(begin(), end());
    }

    auto insert(compact_pointer p) noexcept
    {
      if (auto iter = lower_bound_chunk(p); iter != chunks.end() and iter->offset == p.offset())
      {
        iter->set(p.index());
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
      iter->reset(p.index());
    }

    auto begin() const noexcept
    {
      return iterator(chunks, chunks.begin(), 0);
    }

    auto end() const noexcept
    {
      return iterator(chunks);
    }

    auto lower_bound(compact_pointer p) noexcept
    {
      if (auto iter = lower_bound_chunk(p); iter != chunks.end())
      {
        return iterator(chunks, iter, p.index());
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
