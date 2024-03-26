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

#ifndef INCLUDED_MEEVAX_MEMORY_INTEGER_SET_HPP
#define INCLUDED_MEEVAX_MEMORY_INTEGER_SET_HPP

#include <cassert>
#include <climits> // CHAR_BIT
#include <cstdint>
#include <iterator>
#include <limits>

#include <meevax/bit/bit_cast.hpp>
#include <meevax/bit/log2.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  constexpr auto compressible_bitwidth_of = std::is_pointer_v<T> ? log2(alignof(std::remove_pointer_t<T>)) - 1 : 0;

  template <typename T, std::size_t E, std::size_t... Es>
  struct integer_set
  {
    static_assert(sizeof(T) <= sizeof(std::uintptr_t));

    static_assert(compressible_bitwidth_of<T> < E);

    static constexpr auto N = static_cast<std::size_t>(1) << (E - compressible_bitwidth_of<T>);

    using subset = integer_set<std::uintptr_t, Es...>; // Only the outermost implementation knows the original type name T.

    subset * data[N] = {};

    std::size_t max = 0;

    struct const_iterator : public std::iterator<std::bidirectional_iterator_tag, T>
    {
      subset const* const* data = nullptr;

      std::size_t max = 0;

      std::size_t i = std::numeric_limits<std::size_t>::max();

      typename subset::const_iterator iter;

      constexpr const_iterator() = default;

      explicit const_iterator(integer_set const* container, std::size_t i, std::uintptr_t j = 0) noexcept
        : data { container->data }
        , max  { container->max }
        , i    { i }
      {
        assert(i <= N);
        increment_unless_truthy(j);
      }

      explicit const_iterator(integer_set const* container) noexcept
        : data { container->data }
        , max  { container->max }
        , i    { N }
      {
        decrement_unless_truthy();
        assert(iter.data);
      }

      auto increment_unless_truthy(std::uintptr_t j = 0) noexcept -> void
      {
        assert(data);

        if (good())
        {
          if (data[i] and (iter = data[i]->lower_bound(j)).good())
          {
            return;
          }
          else for (++i; i <= max; ++i)
          {
            if (data[i] and (iter = data[i]->lower_bound(0)).good())
            {
              return;
            }
          }

          i = N;
        }

        iter = {};
      }

      auto decrement_unless_truthy() noexcept -> void
      {
        assert(data);

        for (i = std::min(i, N - 1); good(); --i)
        {
          if (data[i] and (iter = typename subset::const_iterator(data[i])).good())
          {
            return;
          }
        }

        iter = {};
      }

      auto operator ++() noexcept -> auto &
      {
        if (++iter; not iter.good())
        {
          ++i;
          increment_unless_truthy();
        }

        return *this;
      }

      auto operator --() noexcept -> auto &
      {
        if (not iter.data)
        {
          decrement_unless_truthy();
        }
        else if (--iter; not iter.good())
        {
          --i;
          decrement_unless_truthy();
        }

        return *this;
      }

      auto operator *() const noexcept -> T
      {
        assert(good());
        return reinterpret_cast<T>((i << (Es + ...) bitor *iter) << compressible_bitwidth_of<T>);
      }

      auto good() const noexcept -> bool
      {
        assert(data);
        return i < N;
      }

      auto at_end() const noexcept -> bool
      {
        return not data or not good() or iter.at_end();
      }

      auto is_same_index(const_iterator const& other) const noexcept -> bool
      {
        return i == other.i and iter.is_same_index(other.iter);
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b) noexcept
      {
        return a.is_same_index(b) or (a.at_end() and b.at_end());
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not (a == b);
      }
    };

    static constexpr auto split(T p) noexcept
    {
      static_assert(E + (Es + ...) <= sizeof(T) * CHAR_BIT);

      struct big_endian
      {
        std::uintptr_t ignore : sizeof(std::uintptr_t) * CHAR_BIT - E - (Es + ...);
        std::uintptr_t key    : E - compressible_bitwidth_of<T>;
        std::uintptr_t value  : (Es + ...);
        std::uintptr_t        : compressible_bitwidth_of<T>;
      };

      struct little_endian
      {
        std::uintptr_t        : compressible_bitwidth_of<T>;
        std::uintptr_t value  : (Es + ...);
        std::uintptr_t key    : E - compressible_bitwidth_of<T>;
        std::uintptr_t ignore : sizeof(std::uintptr_t) * CHAR_BIT - E - (Es + ...);
      };

      const auto bits = bit_cast<little_endian>(p);

      return std::make_pair(bits.key, bits.value);
    }

    ~integer_set()
    {
      for (auto datum : data)
      {
        delete datum;
      }
    }

    auto insert(T value)
    {
      if (auto [i, j] = split(value); data[i])
      {
        data[i]->insert(j);
      }
      else
      {
        max = std::max(max, i);
        data[i] = new subset();
        data[i]->insert(j);
      }
    }

    auto erase(T value) noexcept
    {
      if (auto [i, j] = split(value); data[i])
      {
        data[i]->erase(j);
      }
    }

    auto contains(T value) noexcept -> bool
    {
      auto [i, j] = split(value);
      return data[i] and data[i]->contains(j);
    }

    auto begin() const noexcept
    {
      return const_iterator(this, 0);
    }

    auto end() const noexcept
    {
      return const_iterator(this, N);
    }

    auto lower_bound(T value) const noexcept
    {
      auto [i, j] = split(value);
      return const_iterator(this, i, j);
    }

    auto size() const noexcept -> std::size_t
    {
      return std::distance(begin(), end());
    }

    auto swap(integer_set & other)
    {
      std::swap(data, other.data);
    }
  };

  template <typename T, std::size_t E>
  struct integer_set<T, E>
  {
    static_assert(std::is_same_v<decltype(0ul), std::uint64_t>);

    static constexpr auto N = 1ul << E;

    std::uint64_t data[N / 64] {};

    struct const_iterator : public std::iterator<std::bidirectional_iterator_tag, T>
    {
      std::uint64_t const* data = nullptr;

      std::size_t index = std::numeric_limits<std::size_t>::max();

      auto increment_unless_truthy() noexcept
      {
        if (auto i = index / 64; i < N/64)
        {
          if (auto datum = data[i] & (~0ul << index % 64); datum)
          {
            index = i * 64 + __builtin_ctzl(datum);
            assert(data[index / 64] & (1ul << index % 64));
            return;
          }
          else while (++i < N/64)
          {
            if (auto datum = data[i]; datum)
            {
              index = i * 64 + __builtin_ctzl(datum);
              assert(data[index / 64] & (1ul << index % 64));
              return;
            }
          }
        }

        index = N;

        assert(not good());
      }

      auto decrement_unless_truthy() noexcept
      {
        if (auto i = index / 64; i < N/64)
        {
          if (auto datum = data[i] & (~0ul >> (63 - index % 64)); datum)
          {
            index = i * 64 + (63 - __builtin_clzl(datum));
            assert(data[index / 64] & (1ul << index % 64));
            return;
          }
          else while (--i < N/64)
          {
            if (auto datum = data[i]; datum)
            {
              index = i * 64 + (63 - __builtin_clzl(datum));
              assert(data[index / 64] & (1ul << index % 64));
              return;
            }
          }
        }

        index = N;

        assert(not good());
      }

      constexpr const_iterator() = default;

      explicit const_iterator(integer_set const* container, std::size_t i) noexcept
        : data  { container->data }
        , index { i }
      {
        increment_unless_truthy();
      }

      explicit const_iterator(integer_set const* container) noexcept
        : data  { container->data }
        , index { N - 1 }
      {
        decrement_unless_truthy();
      }

      auto operator ++() noexcept -> decltype(auto)
      {
        ++index;
        increment_unless_truthy();
        return *this;
      }

      auto operator --() noexcept -> decltype(auto)
      {
        --index;
        decrement_unless_truthy();
        return *this;
      }

      auto operator *() const noexcept
      {
        assert(good());
        return reinterpret_cast<T>(index);
      }

      auto good() const noexcept -> bool
      {
        assert(data);
        return index < N;
      }

      auto at_end() const noexcept -> bool
      {
        return not data or not good();
      }

      auto is_same_index(const_iterator const& other) const noexcept -> bool
      {
        return index == other.index;
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b) noexcept
      {
        return a.is_same_index(b) or (a.at_end() and b.at_end());
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not (a == b);
      }
    };

    auto insert(T value) noexcept
    {
      auto i = reinterpret_cast<std::size_t>(value) / 64;
      auto j = reinterpret_cast<std::size_t>(value) % 64;
      data[i] |= (1ul << j);
    }

    auto erase(T value) noexcept
    {
      auto i = reinterpret_cast<std::size_t>(value) / 64;
      auto j = reinterpret_cast<std::size_t>(value) % 64;
      data[i] &= ~(1ul << j);
    }

    auto contains(T value) noexcept -> bool
    {
      auto i = reinterpret_cast<std::size_t>(value) / 64;
      auto j = reinterpret_cast<std::size_t>(value) % 64;
      return data[i] & (1ul << j);
    }

    auto lower_bound(T value) const noexcept
    {
      return const_iterator(this, reinterpret_cast<std::size_t>(value));
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_INTEGER_SET_HPP
