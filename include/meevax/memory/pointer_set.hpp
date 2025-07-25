/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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
#include <bit>
#include <cassert>
#include <climits> // CHAR_BIT
#include <cstdint>
#include <iterator>
#include <limits>
#include <type_traits>

namespace meevax::inline memory
{
  template <typename T>
  constexpr auto compressible_bitwidth_of = std::is_pointer_v<T> ? std::bit_width(alignof(std::remove_pointer_t<T>)) - 1 : 0;

  constexpr auto operator ""_i64(unsigned long long int value)
  {
    return static_cast<std::int64_t>(value);
  }

  constexpr auto operator ""_u64(unsigned long long int value)
  {
    return static_cast<std::uint64_t>(value);
  }

  template <typename T, std::size_t E, std::size_t... Es>
  struct pointer_set
  {
    static_assert(sizeof(T) <= sizeof(std::uintptr_t));

    static_assert(compressible_bitwidth_of<T> < E);

    static constexpr auto N = static_cast<std::size_t>(1) << (E - compressible_bitwidth_of<T>);

    using subset = pointer_set<std::uintptr_t, Es...>; // Only the outermost implementation knows the original type name T.

    subset * data[N] = {};

    std::size_t i_min = N;
    std::size_t i_max = 0;

    struct const_iterator
    {
      using value_type = T;

      using difference_type = std::ptrdiff_t;

      using pointer = T *;

      using reference = T &;

      using iterator_category = std::bidirectional_iterator_tag;

      pointer_set const* p = nullptr;

      std::size_t i = N;

      typename subset::const_iterator sub;

      constexpr const_iterator() = default;

      explicit const_iterator(pointer_set const* p, std::size_t i, std::uintptr_t j) noexcept
        : p { p }
        , i { i }
      {
        assert(p->data);
        assert(operator bool());
        increment_unless_truthy(j);
      }

      explicit const_iterator(pointer_set const* p, std::size_t i) noexcept
        : p { p }
        , i { i }
      {
        assert(i <= N);
        increment_unless_truthy();
      }

      explicit const_iterator(pointer_set const* p) noexcept
        : p { p }
        , i { N }
      {
        decrement_unless_truthy();
        assert(sub.p->data);
      }

      auto increment_unless_truthy(std::uintptr_t j) noexcept -> void
      {
        assert(p->data);
        assert(operator bool());

        if (not p->data[i] or not (sub = p->data[i]->lower_bound(j)))
        {
          ++i;
          increment_unless_truthy();
        }
      }

      auto increment_unless_truthy() noexcept -> void
      {
        assert(p->data);

        for (; i <= p->i_max; ++i)
        {
          if (p->data[i] and (sub = p->data[i]->lower_bound()))
          {
            return;
          }
        }

        invalidate();
      }

      auto decrement_unless_truthy() noexcept -> void
      {
        assert(decrementable());
        assert(p->data);

        auto const i_min = p->i_min;

        for (i = std::min(i, N - 1); i - i_min < N - i_min; --i)
        {
          assert(i < N);

          if (p->data[i] and (sub = typename subset::const_iterator(p->data[i])))
          {
            return;
          }
        }

        invalidate();
      }

      auto decrementable() const noexcept -> bool
      {
        return p; // NOTE: If `p` is nullptr, this is non-decrementable end iterator (= default constructed).
      }

      auto invalidate()
      {
        /*
           The default constructed iterator (= end iterator) has nullptr set to
           pointer `p`. However, once the pointer `p` is set to non-nullptr, it
           must not be changed.

           This is because an iterator that has reached end as a result of
           being incremented must be able to return to the valid state by
           decrementing it.

           In other words, the invalidity of an iterator depends on whether `i`
           is out of range and whether the sub iterator is invalid.

           Whether or not the pointer `p` is nullptr is used to determine
           whether or not it is decrementable (see the member function
           `decrementable`)
        */
        i = N;
        sub = {};
      }

      auto operator ++() noexcept -> auto &
      {
        if (not ++sub)
        {
          ++i;
          increment_unless_truthy();
        }

        return *this;
      }

      auto operator --() noexcept -> auto &
      {
        if (decrementable() and not --sub and --i < N)
        {
          decrement_unless_truthy();
        }

        return *this;
      }

      constexpr auto operator *() const noexcept -> T
      {
        assert(operator bool());
        return reinterpret_cast<T>((i << (Es + ...) | *sub) << compressible_bitwidth_of<T>);
      }

      constexpr operator bool() const noexcept
      {
        return i < N;
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not b ? not a : a.i == b.i and a.sub == b.sub;
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not (a == b);
      }
    };

    ~pointer_set()
    {
      for (auto datum : data)
      {
        delete datum;
      }
    }

    static constexpr auto split(T value) noexcept -> std::pair<std::size_t, std::uintptr_t>
    {
      auto j = reinterpret_cast<std::uintptr_t>(value) >> compressible_bitwidth_of<T>;
      constexpr auto mask = (1_u64 << E) - 1;
      auto i = j >> (Es + ...) & mask;
      return { i, j };
    }

    auto insert(T value) noexcept
    {
      auto [i, j] = split(value);

      if (not data[i])
      {
        i_min = std::min(i_min, i);
        i_max = std::max(i_max, i);
        data[i] = new subset();
      }

      return data[i]->insert(j);
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

    auto lower_bound() const noexcept
    {
      return const_iterator(this, i_min);
    }

    auto size() const noexcept -> std::size_t
    {
      return std::distance(begin(), end());
    }

    auto swap(pointer_set & other)
    {
      std::swap(data, other.data);
    }
  };

  template <typename T, std::size_t E>
  struct pointer_set<T, E>
  {
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

      pointer_set const* p = nullptr;

      std::size_t i = N;

      auto increment_unless_truthy() noexcept
      {
        if (operator bool())
        {
          auto [q, r] = split(i);

          if (auto b = std::countr_zero(p->data[q] & (~0_u64 << r)); b != 64)
          {
            i = q * 64 + b;
            assert(p->data[i / 64] & (1_u64 << i % 64));
            return;
          }

          while (++q < Q)
          {
            if (auto b = std::countr_zero(p->data[q]); b != 64)
            {
              i = q * 64 + b;
              assert(p->data[i / 64] & (1_u64 << i % 64));
              return;
            }
          }
        }

        i = N;

        assert(not operator bool());
      }

      auto decrement_unless_truthy() noexcept
      {
        if (operator bool())
        {
          auto [q, r] = split(i);

          if (auto b = std::countl_zero(p->data[q] & (~0_u64 >> (R - r))); b != 64)
          {
            i = q * 64 + R - b;
            assert(p->data[i / 64] & (1_u64 << i % 64));
            return;
          }

          while (--q < Q)
          {
            if (auto b = std::countl_zero(p->data[q]); b != 64)
            {
              i = q * 64 + R - b;
              assert(p->data[i / 64] & (1_u64 << i % 64));
              return;
            }
          }
        }

        i = N;

        assert(not operator bool());
      }

      constexpr const_iterator() = default;

      explicit const_iterator(pointer_set const* p, std::size_t i) noexcept
        : p { p }
        , i { i }
      {
        increment_unless_truthy();
      }

      explicit const_iterator(pointer_set const* p) noexcept
        : p { p }
        , i { N - 1 }
      {
        decrement_unless_truthy();
      }

      auto operator ++() noexcept -> decltype(auto)
      {
        ++i;
        increment_unless_truthy();
        return *this;
      }

      auto operator --() noexcept -> decltype(auto)
      {
        --i;
        decrement_unless_truthy();
        return *this;
      }

      constexpr auto operator *() const noexcept
      {
        assert(operator bool());
        return reinterpret_cast<T>(i);
      }

      constexpr operator bool() const noexcept
      {
        return i < N;
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not b ? not a : a.i == b.i;
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b) noexcept
      {
        return not (a == b);
      }
    };

    auto insert(T value) noexcept
    {
      auto [q, r] = split(index(value));
      auto mask = 1_u64 << r;
      auto already_inserted = data[q] & mask;
      data[q] |= mask;
      return not already_inserted;
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
      return const_iterator(this, index(value));
    }

    auto lower_bound() const noexcept
    {
      return const_iterator(this, 0);
    }
  };
} // namespace meevax::memory

#endif // INCLUDED_MEEVAX_MEMORY_POINTER_SET_HPP
