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
#include <memory>
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

    static constexpr auto chunk_size()
    {
      if constexpr (std::is_pointer_v<T>)
      {
        static_assert(compressible_bitwidth_of<T> < E);
        return static_cast<std::size_t>(1) << (E - compressible_bitwidth_of<T>);
      }
      else
      {
        return static_cast<std::size_t>(1) << E;
      }
    }

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

    using superset = std::array<std::unique_ptr<subset>, chunk_size()>;

    superset data {};

    struct const_iterator
    {
      using iterator_category = std::bidirectional_iterator_tag;

      using value_type = T;

      using reference = T const&;

      using pointer = T *;

      using difference_type = std::ptrdiff_t;

      superset const* data = nullptr;

      std::size_t index = std::numeric_limits<std::size_t>::max();

      typename subset::const_iterator iter;

      constexpr const_iterator() = default;

      explicit const_iterator(integer_set const& iset, std::size_t index, std::uintptr_t child_index = 0)
        : data  { std::addressof(iset.data) }
        , index { index }
      {
        assert(index <= chunk_size());
        increment_unless_truthy(child_index);
      }

      explicit const_iterator(integer_set const& iset)
        : data  { std::addressof(iset.data) }
        , index { chunk_size() }
      {
        decrement_unless_truthy();
        assert(iter.data);
      }

      auto out_of_range() const -> bool
      {
        return not data or chunk_size() <= index;
      }

      auto increment_unless_truthy(std::uintptr_t child_index = 0) -> void
      {
        assert(data); // incrementing end iterator

        while (index < chunk_size())
        {
          if ((*data)[index])
          {
            if (iter = (*data)[index]->lower_bound(child_index); not iter.out_of_range())
            {
              return;
            }
          }

          while (++index < chunk_size() and not (*data)[index]);

          child_index = 0;
        }

        iter = {};
      }

      auto decrement_unless_truthy() -> void
      {
        assert(data);

        index = std::min(index, chunk_size() - 1);

        assert(index < chunk_size());

        while (index < chunk_size())
        {
          assert(data);

          if (auto & datum = (*data)[index])
          {
            if (iter = typename subset::const_iterator(*datum); not iter.out_of_range())
            {
              assert(**this);
              return;
            }
          }

          while (--index < chunk_size() and not (*data)[index]);
        }

        assert(data);

        iter = {};
      }

      auto operator ++() -> auto &
      {
        assert(data);

        if (++iter; iter.out_of_range())
        {
          ++index;
          increment_unless_truthy();
        }

        return *this;
      }

      auto operator --() -> auto &
      {
        assert(data);

        if (not iter.data)
        {
          decrement_unless_truthy();
          return *this;
        }
        else
        {
          if (--iter; iter.out_of_range())
          {
            --index;
            decrement_unless_truthy();
          }

          return *this;
        }
      }

      auto operator *() const -> T
      {
        assert(index < chunk_size());

        return decompress(index << (Es + ...) bitor *iter);
      }

      auto is_end() const -> bool
      {
        return out_of_range() or iter.is_end();
      }

      auto is_same_index(const_iterator const& other) const -> bool
      {
        return index == other.index and iter.is_same_index(other.iter);
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b)
      {
        return a.is_same_index(b) or (a.is_end() and b.is_end());
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b)
      {
        return not (a == b);
      }
    };

    static constexpr auto split(T p)
    {
      auto datum = compress(p);

      constexpr std::uintptr_t mask = (chunk_size() - 1) << (Es + ...);

      return std::make_pair((datum &  mask) >> (Es + ...), datum & ~mask);
    }

    auto insert(T value)
    {
      if (auto [key, datum] = split(value); data[key])
      {
        data[key]->insert(datum);
      }
      else
      {
        data[key] = std::make_unique<subset>();
        data[key]->insert(datum);
      }
    }

    auto erase(T value)
    {
      auto [key, datum] = split(value);
      data[key]->erase(datum);
    }

    auto begin() const
    {
      return const_iterator(*this, 0);
    }

    auto end() const
    {
      return const_iterator(*this, chunk_size());
    }

    auto lower_bound(T value) const
    {
      auto [index, child_index] = split(value);
      return const_iterator(*this, index, child_index);
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

    struct const_iterator
    {
      using iterator_category = std::bidirectional_iterator_tag;

      using value_type = bool;

      using reference = bool &;

      using pointer = std::add_pointer_t<value_type>;

      using difference_type = std::ptrdiff_t;

      subset const* data = nullptr;

      std::size_t index = std::numeric_limits<std::size_t>::max();

      auto increment_unless_truthy()
      {
        assert(data);

        while (index < N and not (*data)[index])
        {
          ++index;
        }

        assert(N <= index or (*data)[index]);
      }

      auto decrement_unless_truthy()
      {
        assert(data);

        while (index < N and not (*data)[index])
        {
          --index;
        }

        assert(N <= index or (*data)[index]);
      }

      auto out_of_range() const -> bool
      {
        return not data or N <= index;
      }

      constexpr const_iterator() = default;

      explicit const_iterator(integer_set const& iset, std::size_t index)
        : data  { std::addressof(iset.data) }
        , index { index }
      {
        increment_unless_truthy();
        assert(out_of_range() or (*data)[this->index]);
      }

      explicit const_iterator(integer_set const& iset)
        : data  { std::addressof(iset.data) }
        , index { N - 1 }
      {
        decrement_unless_truthy();
      }

      auto operator ++() -> decltype(auto)
      {
        ++index;
        increment_unless_truthy();
        return *this;
      }

      auto operator --() -> decltype(auto)
      {
        --index;
        decrement_unless_truthy();
        return *this;
      }

      auto operator *() const
      {
        assert(index < N);
        return reinterpret_cast<T>(index);
      }

      auto is_end() const -> bool
      {
        return out_of_range();
      }

      auto is_same_index(const_iterator const& other) const -> bool
      {
        return index == other.index;
      }

      friend auto operator ==(const_iterator const& a, const_iterator const& b)
      {
        return a.is_same_index(b) or (a.is_end() and b.is_end());
      }

      friend auto operator !=(const_iterator const& a, const_iterator const& b)
      {
        return not (a == b);
      }
    };

    template <typename U>
    static constexpr auto split(U value)
    {
      return reinterpret_cast<std::size_t>(value);
    }

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
      return const_iterator(data);
    }

    auto lower_bound(T value) const
    {
      return const_iterator(*this, split(value));
    }

    auto size() const
    {
      return std::distance(begin(), end());
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_INTEGER_SET_HPP
