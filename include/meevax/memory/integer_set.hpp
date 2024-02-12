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

#include <array>
#include <cassert>
#include <iterator>
#include <limits>
#include <type_traits>

#include <meevax/bit/log2.hpp>
#include <meevax/bitset/simple_bitset.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename Pointer>
  constexpr auto compressible_bitwidth_of = log2(alignof(std::remove_pointer_t<Pointer>)) - 1;

  template <typename T, std::size_t E, std::size_t... Es>
  struct integer_set
  {
    static_assert(sizeof(T) <= sizeof(std::uintptr_t));

    static_assert(not std::is_pointer_v<T> or compressible_bitwidth_of<T> < E);

    static constexpr auto N = static_cast<std::size_t>(1) << E - (std::is_pointer_v<T> ? compressible_bitwidth_of<T> : 0);

    static constexpr auto compress(T value)
    {
      if constexpr (std::is_pointer_v<T>)
      {
        return reinterpret_cast<std::uintptr_t>(value) >> compressible_bitwidth_of<T>;
      }
      else
      {
        return reinterpret_cast<std::uintptr_t>(value);
      }
    }

    static constexpr auto decompress(std::uintptr_t value)
    {
      if constexpr (std::is_pointer_v<T>)
      {
        return reinterpret_cast<T>(value << compressible_bitwidth_of<T>);
      }
      else
      {
        return reinterpret_cast<T>(value);
      }
    }

    using subset = integer_set<std::uintptr_t, Es...>; // Only the outermost implementation knows the original type name T.

    using superset = std::array<subset *, N>;

    superset data {};

    struct const_iterator : public std::iterator<std::bidirectional_iterator_tag, T>
    {
      superset const* data = nullptr;

      std::size_t i = std::numeric_limits<std::size_t>::max();

      typename subset::const_iterator iter;

      constexpr const_iterator() = default;

      explicit const_iterator(integer_set const* container, std::size_t i, std::uintptr_t j = 0)
        : data { std::addressof(container->data) }
        , i    { i }
      {
        assert(i <= N);
        increment_unless_truthy(j);
      }

      explicit const_iterator(integer_set const* container)
        : data { std::addressof(container->data) }
        , i    { N }
      {
        decrement_unless_truthy();
        assert(iter.data);
      }

      auto increment_unless_truthy(std::uintptr_t j = 0) -> void
      {
        for (assert(data); good(); ++i, j = 0)
        {
          if (auto datum = (*data)[i]; datum and (iter = datum->lower_bound(j)).good())
          {
            return;
          }
        }

        iter = {};
      }

      auto decrement_unless_truthy() -> void
      {
        i = std::min(i, N - 1);

        assert(good());

        for (assert(data); good(); --i)
        {
          if (auto datum = (*data)[i]; datum and (iter = typename subset::const_iterator(datum)).good())
          {
            return;
          }
        }

        iter = {};
      }

      auto operator ++() -> auto &
      {
        if (++iter; not iter.good())
        {
          ++i;
          increment_unless_truthy();
        }

        return *this;
      }

      auto operator --() -> auto &
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

      auto operator *() const -> T
      {
        assert(good());
        return decompress(i << (Es + ...) bitor *iter);
      }

      auto good() const noexcept -> bool
      {
        assert(data);
        return i < N;
      }

      auto at_end() const -> bool
      {
        return not data or not good() or iter.at_end();
      }

      auto is_same_index(const_iterator const& other) const -> bool
      {
        return i == other.i and iter.is_same_index(other.iter);
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b)
      {
        return a.is_same_index(b) or (a.at_end() and b.at_end());
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b)
      {
        return not (a == b);
      }
    };

    static constexpr auto split(T p)
    {
      auto datum = compress(p);

      constexpr std::uintptr_t mask = (N - 1) << (Es + ...);

      return std::make_pair((datum & mask) >> (Es + ...), datum & ~mask);
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
      if (auto [key, datum] = split(value); data[key])
      {
        data[key]->insert(datum);
      }
      else
      {
        data[key] = new subset();
        data[key]->insert(datum);
      }
    }

    auto erase(T value)
    {
      auto [i, j] = split(value);
      data[i]->erase(j);
    }

    auto begin() const
    {
      return const_iterator(this, 0);
    }

    auto end() const
    {
      return const_iterator(this, N);
    }

    auto lower_bound(T value) const
    {
      auto [i, j] = split(value);
      return const_iterator(this, i, j);
    }

    auto size() const -> std::size_t
    {
      return std::distance(begin(), end());
    }
  };

  template <typename T, std::size_t E>
  struct integer_set<T, E>
  {
    static constexpr auto N = static_cast<std::size_t>(1) << E;

    using subset = simple_bitset<N>;

    subset data {};

    struct const_iterator : public std::iterator<std::bidirectional_iterator_tag, T>
    {
      subset const* data = nullptr;

      std::size_t i = std::numeric_limits<std::size_t>::max();

      auto increment_unless_truthy()
      {
        for (assert(data); good() and not (*data)[i]; ++i)
        {}

        assert(not good() or (*data)[i]);
      }

      auto decrement_unless_truthy()
      {
        for (assert(data); good() and not (*data)[i]; --i)
        {}

        assert(not good() or (*data)[i]);
      }

      constexpr const_iterator() = default;

      explicit const_iterator(integer_set const* container, std::size_t i)
        : data { std::addressof(container->data) }
        , i    { i }
      {
        increment_unless_truthy();
        assert(not good() or (*data)[this->i]);
      }

      explicit const_iterator(integer_set const* container)
        : data { std::addressof(container->data) }
        , i    { N - 1 }
      {
        decrement_unless_truthy();
      }

      auto operator ++() -> decltype(auto)
      {
        ++i;
        increment_unless_truthy();
        return *this;
      }

      auto operator --() -> decltype(auto)
      {
        --i;
        decrement_unless_truthy();
        return *this;
      }

      auto operator *() const
      {
        assert(good());
        return reinterpret_cast<T>(i);
      }

      auto good() const noexcept -> bool
      {
        assert(data);
        return i < N;
      }

      auto at_end() const -> bool
      {
        return not data or not good();
      }

      auto is_same_index(const_iterator const& other) const -> bool
      {
        return i == other.i;
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b)
      {
        return a.is_same_index(b) or (a.at_end() and b.at_end());
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b)
      {
        return not (a == b);
      }
    };

    auto insert(T value)
    {
      data.set(reinterpret_cast<std::size_t>(value));
    }

    auto erase(T value)
    {
      data.reset(reinterpret_cast<std::size_t>(value));
    }

    auto begin() const
    {
      return const_iterator(data, 0);
    }

    auto end() const
    {
      return const_iterator(data, N);
    }

    auto lower_bound(T value) const
    {
      return const_iterator(this, reinterpret_cast<std::size_t>(value));
    }

    auto size() const
    {
      return std::distance(begin(), end());
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_INTEGER_SET_HPP
