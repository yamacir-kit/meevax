/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_MEMORY_UNSIGNED_INTEGER_SET_HPP
#define INCLUDED_MEEVAX_MEMORY_UNSIGNED_INTEGER_SET_HPP

#include <algorithm>
#include <cstdint>
#include <iterator>
#include <limits>
#include <vector>

namespace meevax
{
inline namespace memory
{
  template <typename N>
  constexpr auto log2(N n) -> std::size_t
  {
    return n <= 2 ? 1 : log2(n / 2) + 1;
  }

  class unsigned_integer_set
  {
    std::size_t size;

    std::vector<std::uint64_t> data;

    static constexpr auto digits = std::numeric_limits<std::uint64_t>::digits;

    constexpr auto index_of(std::size_t value)
    {
      return value / digits;
    };

    constexpr auto mark_of(std::size_t value)
    {
      return static_cast<std::uint64_t>(1) << (value - digits * index_of(value));
    };

  public:
    struct iterator
    {
      using iterator_category = std::forward_iterator_tag;

      using value_type = std::uint64_t;

      using difference_type = std::ptrdiff_t;

      std::uint64_t const* data;

      std::size_t size;

      std::size_t index;

      std::size_t bit;

      explicit iterator(std::vector<std::uint64_t> const& bitset)
        : data { bitset.data() }
        , size { bitset.size() }
        , index { 0 }
        , bit { 0 }
      {
        if (not valid())
        {
          operator ++();
        }
      }

      explicit iterator()
        : data { nullptr }
        , size { 0 }
        , index { std::numeric_limits<std::size_t>::max() }
        , bit { 0 }
      {}

      auto valid() const -> bool
      {
        return (data[index] >> bit) & 1;
      }

      auto operator *() const
      {
        return index * digits + bit;
      }

      auto operator ++() -> iterator &
      {
        do
        {
          if (digits <= ++bit)
          {
            bit = 0;

            if (size <= ++index)
            {
              index = std::numeric_limits<std::size_t>::max();
              return *this;
            }
          }
        }
        while (not valid());

        return *this;
      }

      auto operator ++(int) -> auto
      {
        auto copy = *this;
        operator ++();
        return copy;
      }

      auto operator ==(iterator const& rhs)
      {
        return index == rhs.index and bit == rhs.bit;
      }

      auto operator !=(iterator const& rhs)
      {
        return not operator ==(rhs);
      }
    };

    explicit unsigned_integer_set()
      : size { 0 }
      , data {}
    {}

    auto resize(std::size_t given_size)
    {
      size = given_size;
      data.resize(size / digits + 1);
    }

    auto insert(std::uint64_t value)
    {
      if (data.size() < index_of(value))
      {
        resize(value);
      }

      data[index_of(value)] |= mark_of(value);
    }

    auto erase(std::uint64_t value)
    {
      data[index_of(value)] &= ~mark_of(value);
    }

    auto erase(iterator iter)
    {
      erase(*iter);
      return ++iter;
    }

    auto clear()
    {
      std::fill(std::begin(data), std::end(data), 0);
    }

    auto begin() const
    {
      return iterator(data);
    }

    auto end() const
    {
      return iterator();
    }

    template <typename... Ts>
    auto lower_bound(Ts&&... xs)
    {
      return std::lower_bound(begin(), end(), std::forward<decltype(xs)>(xs)...);
    }

    template <typename... Ts>
    auto upper_bound(Ts&&... xs)
    {
      return std::upper_bound(begin(), end(), std::forward<decltype(xs)>(xs)...);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_UNSIGNED_INTEGER_SET_HPP
