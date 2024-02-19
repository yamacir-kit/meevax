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
#include <unordered_map>

#include <meevax/bit/log2.hpp>
#include <meevax/bitset/simple_bitset.hpp>
#include <meevax/iterator/index_iterator.hpp>
#include <meevax/map/simple_flat_map.hpp>
#include <meevax/memory/integer_set.hpp>

namespace meevax
{
inline namespace memory
{
inline namespace v1
{
  template <typename Pointer,
            template <typename...> typename OrderedMap = simple_flat_map,
            template <std::size_t> typename Bitset = simple_bitset,
            std::size_t N = 4096 * 8> // getconf PAGE_SIZE
  class [[deprecated]] pointer_set
  {
    static_assert(std::is_pointer_v<Pointer>);

    static_assert(N % (sizeof(std::uintptr_t) * 8) == 0);

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
      using const_iterator = index_iterator<chunk>;

      explicit chunk(std::size_t const& index)
      {
        Bitset<N>::set(index);
      }

      auto begin() const
      {
        return const_iterator(*this, 0);
      }

      auto end() const
      {
        return const_iterator(*this, this->size());
      }
    };

    OrderedMap<std::size_t, chunk> chunks;

  public:
    struct iterator
    {
      using iterator_category = std::bidirectional_iterator_tag;

      using value_type = Pointer;

      using reference = std::add_lvalue_reference_t<value_type>;

      using pointer = std::add_pointer_t<value_type>;

      using difference_type = std::ptrdiff_t;

      OrderedMap<std::size_t, chunk> const& chunks;

      typename OrderedMap<std::size_t, chunk>::const_iterator outer;

      typename chunk::const_iterator inner;

      explicit iterator(OrderedMap<std::size_t, chunk> const& chunks,
                        typename OrderedMap<std::size_t, chunk>::const_iterator outer,
                        std::size_t hint)
        : chunks { chunks }
        , outer  { outer }
        , inner  { outer != chunks.end() ? typename chunk::const_iterator(outer->second, hint)
                                         : typename chunk::const_iterator() }
      {
        if (not dereferenceable() and incrementable())
        {
          operator ++();
        }
      }

      explicit iterator(OrderedMap<std::size_t, chunk> const& chunks)
        : chunks { chunks }
        , outer  { chunks.end() }
        , inner  {}
      {
        assert(not dereferenceable());
      }

      auto incrementable() const -> bool
      {
        return outer != chunks.end() and inner != outer->second.end();
      }

      auto decrementable() const -> bool
      {
        return outer != chunks.begin() or inner != outer->second.begin();
      }

      auto dereferenceable() const -> bool
      {
        return incrementable() and *inner;
      }

      auto operator *() const
      {
        assert(dereferenceable());
        return compact_pointer::to_pointer(outer->first + inner.index);
      }

      auto operator ++() -> auto &
      {
        /*
           NOTE: Incrementing the end iterator is undefined behavior, so there
           is no need to consider that case.
        */
        assert(incrementable());

        for (++inner; outer != chunks.end(); inner = (++outer)->second.begin())
        {
          for (; inner != outer->second.end(); ++inner)
          {
            if (*inner)
            {
              return *this;
            }
          }
        }

        inner = typename chunk::const_iterator();

        assert(not dereferenceable());

        return *this; // end
      }

      auto operator ++(int)
      {
        auto copy = *this;
        operator ++();
        return copy;
      }

      auto operator --() -> auto &
      {
        while (true)
        {
          assert(decrementable());

          if (outer == chunks.end() or inner == outer->second.begin())
          {
            inner = std::prev((--outer)->second.end());
          }
          else
          {
            --inner;
          }

          assert(incrementable());

          if (*inner)
          {
            return *this;
          }
        }
      }

      auto operator --(int)
      {
        auto copy = *this;
        operator --();
        return copy;
      }

      auto operator ==(iterator const& rhs) const
      {
        return outer == rhs.outer and inner == rhs.inner;
      }

      auto operator !=(iterator const& rhs) const
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

    auto size() const -> std::size_t
    {
      return std::distance(begin(), end());
    }

    auto insert(compact_pointer p)
    {
      if (auto iter = chunks.lower_bound(p.offset()); iter != chunks.end() and iter->first == p.offset())
      {
        iter->second.set(p.index());
      }
      else
      {
        assert(iter == chunks.end() or p.offset() < iter->first);
        chunks.emplace_hint(iter, p.offset(), p.index());
      }
    }

    auto erase(compact_pointer p)
    {
      auto iter = chunks.lower_bound(p.offset());
      assert(iter != chunks.end());
      iter->second.reset(p.index());
    }

    auto begin() const
    {
      return iterator(chunks, chunks.begin(), 0);
    }

    auto end() const
    {
      return iterator(chunks);
    }

    auto lower_bound(compact_pointer p)
    {
      if (auto iter = chunks.lower_bound(p.offset()); iter != chunks.end())
      {
        return iterator(chunks, iter, p.index());
      }
      else
      {
        return end();
      }
    }
  };
} // namespace v1

namespace v2
{
  template <typename Pointer>
  struct pointer_set : integer_set<Pointer, 16, 16, 16>
  {};
} // namespace v2
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_POINTER_SET_HPP
