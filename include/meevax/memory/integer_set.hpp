/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#include <algorithm>
#include <bit>
#include <cassert>
#include <climits> // CHAR_BIT
#include <cstdint>
#include <iterator>
#include <limits>

namespace meevax::inline memory
{
  template <typename T>
  constexpr auto compressible_bitwidth_of = std::is_pointer_v<T> ? std::bit_width(alignof(std::remove_pointer_t<T>)) - 1 : 0;

  constexpr auto operator ""_u64(unsigned long long int value)
  {
    return static_cast<std::uint64_t>(value);
  }

  template <typename T, std::size_t E, std::size_t... Es>
  struct integer_set
  {
    static_assert(sizeof(T) <= sizeof(std::uintptr_t));

    static_assert(compressible_bitwidth_of<T> < E);

    static constexpr auto N = static_cast<std::size_t>(1) << (E - compressible_bitwidth_of<T>);

    using subset = integer_set<std::uintptr_t, Es...>; // Only the outermost implementation knows the original type name T.

    subset * data[N] = {};

    static inline std::size_t hint = 0;

    struct const_iterator
    {
      using value_type = T;

      using difference_type = std::ptrdiff_t;

      using pointer = T *;

      using reference = T &;

      using iterator_category = std::bidirectional_iterator_tag;

      subset const* const* data = nullptr;

      std::size_t i = std::numeric_limits<std::size_t>::max();

      typename subset::const_iterator iter;

      constexpr const_iterator() = default;

      explicit const_iterator(subset const* const* data, std::size_t i, std::uintptr_t j) noexcept
        : data { data }
        , i    { i }
      {
        assert(i <= N);
        increment_unless_truthy(j);
      }

      explicit const_iterator(subset const* const* data, std::size_t i) noexcept
        : data { data }
        , i    { i }
      {
        assert(i <= N);
        increment_unless_truthy();
      }

      explicit const_iterator(subset const* const* data) noexcept
        : data { data }
        , i    { N }
      {
        decrement_unless_truthy();
        assert(iter.data);
      }

      auto increment_unless_truthy(std::uintptr_t j) noexcept -> void
      {
        assert(data);

        if (not operator bool() or not data[i] or not (iter = data[i]->lower_bound(j)))
        {
          ++i;
          increment_unless_truthy();
        }
      }

      auto increment_unless_truthy() noexcept -> void
      {
        assert(data);

        if (operator bool())
        {
          for (; i <= hint; ++i)
          {
            if (data[i] and (iter = data[i]->lower_bound(0)))
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

        for (i = std::min(i, N - 1); operator bool(); --i)
        {
          if (data[i] and (iter = typename subset::const_iterator(data[i]->data)))
          {
            return;
          }
        }

        iter = {};
      }

      auto operator ++() noexcept -> auto &
      {
        if (not ++iter)
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
        else if (not --iter)
        {
          --i;
          decrement_unless_truthy();
        }

        return *this;
      }

      constexpr auto operator *() const noexcept -> T
      {
        assert(operator bool());
        return reinterpret_cast<T>((i << (Es + ...) | *iter) << compressible_bitwidth_of<T>);
      }

      constexpr operator bool() const noexcept
      {
        return i < N;
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not b ? not a : a.i == b.i and a.iter == b.iter;
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not (a == b);
      }
    };

    ~integer_set()
    {
      for (auto datum : data)
      {
        delete datum;
      }
    }

    static constexpr auto split(T value) noexcept
    {
      auto j = reinterpret_cast<std::uintptr_t>(value) >> compressible_bitwidth_of<T>;
      constexpr auto mask = (static_cast<std::size_t>(1) << E) - 1;
      auto i = j >> (Es + ...) & mask;
      return std::make_pair(i, j);
    }

    auto insert(T value) noexcept
    {
      auto [i, j] = split(value);

      if (not data[i])
      {
        hint = std::max(hint, i);
        data[i] = new subset();
      }

      data[i]->insert(j);
    }

    auto erase(T value) noexcept
    {
      auto [i, j] = split(value);
      assert(data[i]);
      data[i]->erase(j);
    }

    constexpr auto contains(T value) const noexcept -> bool
    {
      auto [i, j] = split(value);
      return data[i] and data[i]->contains(j);
    }

    auto begin() const noexcept
    {
      return const_iterator(data, 0);
    }

    auto end() const noexcept
    {
      return const_iterator(data, N);
    }

    auto lower_bound(T value) const noexcept
    {
      auto [i, j] = split(value);
      return const_iterator(data, i, j);
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
    static_assert(std::is_same_v<decltype(0_u64), std::uint64_t>);

    static constexpr auto N = 1_u64 << E;

    static constexpr auto Q = N / 64;

    static constexpr auto R = 63;

    std::uint64_t data[Q] {};

    static constexpr auto split = [](auto x)
    {
      return std::make_pair(reinterpret_cast<std::size_t>(x) / 64,
                            reinterpret_cast<std::size_t>(x) % 64);
    };

    static constexpr auto index = [](auto x)
    {
      constexpr auto mask = (static_cast<std::size_t>(1) << E) - 1;
      return reinterpret_cast<std::size_t>(x) >> compressible_bitwidth_of<T> & mask;
    };

    struct const_iterator
    {
      using value_type = T;

      using difference_type = std::ptrdiff_t;

      using pointer = T *;

      using reference = T &;

      using iterator_category = std::bidirectional_iterator_tag;

      std::uint64_t const* data = nullptr;

      std::size_t index = std::numeric_limits<std::size_t>::max();

      auto increment_unless_truthy() noexcept
      {
        if (operator bool())
        {
          auto [q, r] = split(index);

          if (auto b = std::countr_zero(data[q] & (~0_u64 << r)); b != 64)
          {
            index = q * 64 + b;
            assert(data[index / 64] & (1_u64 << index % 64));
            return;
          }

          while (++q < Q)
          {
            if (auto b = std::countr_zero(data[q]); b != 64)
            {
              index = q * 64 + b;
              assert(data[index / 64] & (1_u64 << index % 64));
              return;
            }
          }
        }

        index = N;

        assert(not operator bool());
      }

      auto decrement_unless_truthy() noexcept
      {
        if (operator bool())
        {
          auto [q, r] = split(index);

          if (auto b = std::countl_zero(data[q] & (~0_u64 >> (R - r))); b != 64)
          {
            index = q * 64 + R - b;
            assert(data[index / 64] & (1_u64 << index % 64));
            return;
          }

          while (--q < Q)
          {
            if (auto b = std::countl_zero(data[q]); b != 64)
            {
              index = q * 64 + R - b;
              assert(data[index / 64] & (1_u64 << index % 64));
              return;
            }
          }
        }

        index = N;

        assert(not operator bool());
      }

      constexpr const_iterator() = default;

      explicit const_iterator(std::uint64_t const* data, std::size_t index) noexcept
        : data  { data }
        , index { index }
      {
        increment_unless_truthy();
      }

      explicit const_iterator(std::uint64_t const* data) noexcept
        : data  { data }
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

      constexpr auto operator *() const noexcept
      {
        assert(operator bool());
        return reinterpret_cast<T>(index);
      }

      constexpr operator bool() const noexcept
      {
        return index < N;
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not b ? not a : a.index == b.index;
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not (a == b);
      }
    };

    auto insert(T value) noexcept
    {
      auto [q, r] = split(index(value));
      data[q] |= (1_u64 << r);
    }

    auto erase(T value) noexcept
    {
      auto [q, r] = split(index(value));
      data[q] &= ~(1_u64 << r);
    }

    auto contains(T value) noexcept -> bool
    {
      auto [q, r] = split(index(value));
      return data[q] & (1_u64 << r);
    }

    auto lower_bound(T value) const noexcept
    {
      return const_iterator(data, index(value));
    }
  };
} // namespace meevax::memory

#endif // INCLUDED_MEEVAX_MEMORY_INTEGER_SET_HPP
