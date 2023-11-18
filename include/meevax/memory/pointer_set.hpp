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
#include <optional>
#include <type_traits>
#include <vector>

#include <meevax/bit/log2.hpp>
#include <meevax/bitset/simple_bitset.hpp>
#include <meevax/iterator/naive_index_iterator.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename Pointer,
            template <std::size_t> typename Bitset = simple_bitset,
            std::size_t N = 4096 * 8> // getconf PAGE_SIZE
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

      auto begin() const
      {
        return naive_index_iterator(*this, 0);
      }

      auto end() const
      {
        return naive_index_iterator(*this);
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

      typename std::vector<chunk>::const_iterator outer;

      std::optional<naive_index_iterator<chunk>> inner;

      explicit iterator(std::vector<chunk> const& chunks,
                        typename std::vector<chunk>::const_iterator iter,
                        std::size_t hint) noexcept
        : chunks { chunks }
        , outer  { iter }
        , inner  { outer != chunks.end() ? std::make_optional(naive_index_iterator(*outer, hint)) : std::nullopt }
      {
        if (not dereferenceable() and incrementable())
        {
          operator ++();
        }
      }

      explicit iterator(std::vector<chunk> const& chunks) noexcept
        : chunks { chunks }
        , outer  { chunks.end() }
        , inner  { std::nullopt }
      {
        assert(not dereferenceable());
      }

      auto incrementable() const -> bool
      {
        return outer != chunks.end() and inner and inner != outer->end();
      }

      auto decrementable() const -> bool
      {
        return outer != chunks.begin() or not inner or inner != outer->begin();
      }

      auto dereferenceable() const -> bool
      {
        return incrementable() and **inner;
      }

      auto operator *() const noexcept
      {
        assert(dereferenceable());
        return compact_pointer::to_pointer(outer->offset + inner->index);
      }

      auto operator ++() noexcept -> auto &
      {
        /*
           NOTE: Incrementing the end iterator is undefined behavior, so there
           is no need to consider that case.
        */
        assert(incrementable());

        for (++*inner; outer != chunks.end(); inner = (++outer)->begin())
        {
          for (; inner != outer->end(); ++*inner)
          {
            if (**inner)
            {
              return *this;
            }
          }
        }

        inner = std::nullopt;

        assert(not dereferenceable());

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
        while (true)
        {
          assert(decrementable());

          if (outer == chunks.end() or inner == outer->begin())
          {
            inner = std::prev((--outer)->end());
          }
          else
          {
            --*inner;
          }

          assert(incrementable());

          if (**inner)
          {
            return *this;
          }
        }
      }

      auto operator --(int) noexcept
      {
        auto copy = *this;
        operator --();
        return copy;
      }

      auto operator ==(iterator const& rhs) const noexcept
      {
        return outer == rhs.outer and inner == rhs.inner;
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
