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

#ifndef INCLUDED_MEEVAX_MEMORY_MULTICHANNEL_POINTER_SET_HPP
#define INCLUDED_MEEVAX_MEMORY_MULTICHANNEL_POINTER_SET_HPP

#include <algorithm>
#include <bit>
#include <concepts>
#include <limits>
#include <memory>
#include <meevax/memory/bit_matrix.hpp>
#include <meevax/memory/trie.hpp>
#include <meevax/utility/tuple_index.hpp>

namespace meevax::inline memory
{
  template <typename... Ts>
  struct multichannel_pointer_set
  {
    static_assert(0 < sizeof...(Ts));

    static_assert((std::is_object_v<Ts> and ...));

    static_assert((not std::is_pointer_v<Ts> and ...));

    auto static constexpr A = std::min({ alignof(Ts)... });

    template <typename T>
    auto static constexpr row_of = tuple_index_v<std::remove_cvref_t<T>, std::tuple<Ts...>>;

    auto static constexpr Row = sizeof...(Ts);

    auto static constexpr Column = std::size_t(1) << 14;

    auto static constexpr L1 = std::size_t(1) << 16;

    auto static constexpr L2 = (std::size_t(1) << (47 - std::countr_zero(Column) - std::countr_zero(L1))) >> std::countr_zero(A);

    trie<bit_matrix<Row, Column>, L1, L2> data {};

    auto static constexpr to_index(std::uintptr_t n) noexcept
    {
      assert(n % A == 0);
      return n >> std::countr_zero(A);
    }

    auto static to_index(void const* p) noexcept
    {
      return to_index(reinterpret_cast<std::uintptr_t>(p));
    }

    template <typename P>
    auto static to_pointer(std::uintptr_t i) noexcept
    {
      return reinterpret_cast<P>(i << std::countr_zero(A));
    }

    template <std::size_t R = 0, std::size_t... Rs>
    struct iterator
    {
      using value_type = std::add_pointer_t<std::add_const_t<std::tuple_element_t<R, std::tuple<Ts...>>>>;

      using difference_type = std::ptrdiff_t;

      using iterator_concept = std::forward_iterator_tag;

      typename decltype(data)::iterator chunk {};

      typename bit_matrix<Row, Column>::template iterator<R, Rs...> column {};

      auto constexpr seek() noexcept
      {
        while (column == std::default_sentinel and ++chunk != std::default_sentinel)
        {
          column = chunk->template slice<R, Rs...>().begin();
        }
      }

      constexpr iterator() noexcept = default;

      constexpr iterator(typename decltype(data)::iterator chunk, typename bit_matrix<Row, Column>::template iterator<R, Rs...> column) noexcept
        : chunk { chunk }
        , column { column }
      {}

      explicit constexpr iterator(typename decltype(data)::iterator chunk, std::uintptr_t i = 0) noexcept
        : chunk { chunk }
      {
        if (chunk != std::default_sentinel)
        {
          column = chunk->template slice<R, Rs...>().lower_bound(chunk.i == i / Column ? i % Column : 0);

          seek();
        }
      }

      auto operator *() const noexcept
      {
        return to_pointer<value_type>(chunk.i * Column + *column);
      }

      template <typename T>
      auto constexpr contains() const noexcept
      {
        return column.test(row_of<T>);
      }

      auto constexpr operator ++() noexcept -> iterator &
      {
        ++column;
        seek();
        return *this;
      }

      auto constexpr operator ++(int) noexcept -> iterator
      {
        return std::exchange(*this, std::next(*this));
      }

      auto constexpr friend operator ==(iterator const& a, iterator const& b) noexcept -> bool
      {
        return a == std::default_sentinel ? b == std::default_sentinel
                                          : b != std::default_sentinel and a.chunk == b.chunk and a.column == b.column;
      }

      auto constexpr friend operator ==(iterator const& a, std::default_sentinel_t) noexcept -> bool
      {
        return a.chunk == std::default_sentinel;
      }
    };

    constexpr multichannel_pointer_set() noexcept = default;

    multichannel_pointer_set(multichannel_pointer_set const&) = delete;

    auto operator =(multichannel_pointer_set const&) -> multichannel_pointer_set & = delete;

    template <typename T>
    auto constexpr contains(T const* p) const noexcept
    {
      auto const [q, r] = qr(to_index(p), Column);
      auto const node = data.find(q);
      return node != data.end() and (*node)(row_of<T>, r);
    }

    template <typename T>
    auto constexpr insert(T const* p)
    {
      auto const [q, r] = qr(to_index(p), Column);
      data[q](row_of<T>, r) = true;
    }

    template <typename T>
    auto constexpr erase(T const* p) noexcept
    {
      auto const [q, r] = qr(to_index(p), Column);
      auto const node = data.find(q);
      return node != data.end() and (*node)(row_of<T>, r).exchange(false);
    }

    template <typename T>
    auto constexpr insert(T const* p, std::size_t n)
    {
      auto const first = to_index(p);

      auto const last = first + to_index(n);

      auto const [q, r] = qr(first, Column);

      if (first < last and q == (last - 1) / Column) [[likely]]
      {
        data[q](row_of<T>, r, r + last - first) = true;
      }
      else
      {
        auto k = r;

        auto const end = first == last ? q : (last - 1) / Column + 1;

        for (auto i = q; i < end; ++i)
        {
          data[i](row_of<T>, std::exchange(k, 0), std::min(last - i * Column, Column)) = true;
        }
      }
    }

    template <typename T>
    auto constexpr erase(T const* p, std::size_t n) noexcept
    {
      auto const first = to_index(p);

      auto const last = first + to_index(n);

      for (auto iterator = data.lower_bound(first / Column); first < last and iterator != data.end() and iterator.i * Column < last; ++iterator)
      {
        auto const i = iterator.i * Column;

        (*iterator)(row_of<T>, std::max(first, i) - i, std::min(last - i, Column)) = false;
      }
    }

    auto constexpr swap(multichannel_pointer_set & other) noexcept
    {
      data.swap(other.data);
    }

    auto constexpr clear() noexcept
    {
      data.clear();
    }

    auto constexpr begin() const noexcept
    {
      return iterator<0>(data.begin());
    }

    template <typename T>
    auto constexpr begin() const noexcept
    {
      return iterator<row_of<T>>(data.begin());
    }

    template <typename T, typename U>
    auto constexpr begin() const noexcept
    {
      return iterator<row_of<T>, row_of<U>>(data.begin());
    }

    auto constexpr end() const noexcept -> std::default_sentinel_t
    {
      return {};
    }

    template <typename T>
    auto constexpr lower_bound(T const* p) const noexcept
    {
      auto const i = to_index(p);
      return iterator<row_of<T>>(data.lower_bound(i / Column), i);
    }

    template <typename T, typename U>
    auto constexpr difference() const noexcept
    {
      return std::ranges::subrange(begin<T, U>(), end());
    }
  };
} // namespace meevax::memory

#endif // INCLUDED_MEEVAX_MEMORY_MULTICHANNEL_POINTER_SET_HPP
