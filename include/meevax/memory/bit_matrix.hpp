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

#ifndef INCLUDED_MEEVAX_MEMORY_BIT_MATRIX_HPP
#define INCLUDED_MEEVAX_MEMORY_BIT_MATRIX_HPP

#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <meevax/memory/qr.hpp>
#include <ranges>
#include <span>
#include <type_traits>

namespace meevax::inline memory
{
  template <std::size_t Row, std::size_t Column>
  struct alignas(64) bit_matrix
  {
    static_assert(0 < Row);

    static_assert(0 < Column);

    static_assert(Column % 64 == 0);

    using value_type = std::array<std::uint64_t, Column / 64>;

    std::array<value_type, Row> data {};

    template <typename Word>
    struct bit_proxy
    {
      Word & word;

      std::uint64_t mask;

      constexpr bit_proxy(Word & word, std::uint64_t mask) noexcept
        : word { word }
        , mask { mask }
      {}

      constexpr bit_proxy(bit_proxy const&) noexcept = default;

      constexpr operator bool() const noexcept
      {
        return word & mask;
      }

      auto constexpr operator =(bool b) noexcept -> bit_proxy & requires (not std::is_const_v<Word>)
      {
        word = b ? word | mask : word & ~mask;
        return *this;
      }

      auto constexpr operator =(bit_proxy const& b) noexcept -> bit_proxy & requires (not std::is_const_v<Word>)
      {
        return *this = bool(b);
      }

      auto constexpr operator ~() const noexcept -> bool
      {
        return not bool(*this);
      }

      auto constexpr exchange(bool b) noexcept -> bool requires (not std::is_const_v<Word>)
      {
        bool const old = *this;
        *this = b;
        return old;
      }
    };

    template <typename Word>
    struct bit_range_proxy
    {
      std::span<Word> word;

      std::uint64_t mask1, mask2;

      auto operator =(bit_range_proxy const&) -> bit_range_proxy & = delete;

      auto constexpr operator =(bool b) noexcept -> bit_range_proxy & requires (not std::is_const_v<Word>)
      {
        if (word.size() == 1)
        {
          bit_proxy { word.front(), mask1 & mask2 } = b;
        }
        else
        {
          bit_proxy { word.front(), mask1 } = b;
          std::ranges::fill(word.subspan(1, word.size() - 2), b ? ~std::uint64_t(0) : 0);
          bit_proxy { word.back(), mask2 } = b;
        }

        return *this;
      }
    };

    template <std::size_t R1, std::size_t R2 = Row>
    struct iterator
    {
      static_assert(R1 < Row);

      static_assert(R2 <= Row);

      using value_type = std::size_t;

      using difference_type = std::ptrdiff_t;

      using iterator_concept = std::forward_iterator_tag;

      bit_matrix const* matrix = nullptr;

      std::size_t j = Column / 64;

      std::uint64_t word = 0;

      auto constexpr read_word() const noexcept
      {
        if constexpr (R2 < Row)
        {
          return (*matrix)[R1][j] & ~(*matrix)[R2][j];
        }
        else
        {
          return (*matrix)[R1][j];
        }
      }

      auto constexpr seek() noexcept
      {
        while (not word and ++j < Column / 64)
        {
          word = read_word();
        }
      }

      constexpr iterator() noexcept = default;

      constexpr iterator(bit_matrix const* matrix, std::size_t k) noexcept
        : matrix { matrix }
        , j { k / 64 }
      {
        assert(k <= Column);

        if (j < Column / 64)
        {
          word = read_word() & (~std::uint64_t(0) << (k % 64));
          seek();
        }
      }

      auto constexpr operator *() const noexcept -> value_type
      {
        assert(word);
        return j * 64 + std::countr_zero(word);
      }

      auto constexpr test(std::size_t i) const noexcept -> bool
      {
        assert(word);
        assert(i < Row);
        return (*matrix)[i][j] & (word & -word);
      }

      auto constexpr operator ++() noexcept -> auto &
      {
        assert(word);

        if (word &= word - 1; not word)
        {
          seek();
        }

        return *this;
      }

      auto constexpr operator ++(int) noexcept -> iterator
      {
        auto previous = *this;
        ++*this;
        return previous;
      }

      auto constexpr friend operator ==(iterator const& a, iterator const& b) noexcept
      {
        return a.matrix == b.matrix and a.j == b.j and (not a.word ? not b.word : b.word and *a == *b);
      }

      auto constexpr friend operator ==(iterator const& a, std::default_sentinel_t) noexcept
      {
        return not a.word;
      }
    };

    template <std::size_t R1, std::size_t R2 = Row>
    struct slice_view : std::ranges::view_interface<slice_view<R1, R2>>
    {
      static_assert(R1 < Row);

      static_assert(R2 <= Row);

      bit_matrix const* matrix;

      explicit constexpr slice_view(bit_matrix const* matrix) noexcept
        : matrix { matrix }
      {}

      auto constexpr begin() const noexcept
      {
        return lower_bound(0);
      }

      auto constexpr end() const noexcept -> std::default_sentinel_t
      {
        return {};
      }

      auto constexpr lower_bound(std::size_t k) const noexcept
      {
        return iterator<R1, R2>(matrix, k);
      }

      auto constexpr test(std::size_t j) const noexcept -> bool
      {
        if constexpr (R2 < Row)
        {
          return (*matrix)(R1, j) and not (*matrix)(R2, j);
        }
        else
        {
          return (*matrix)(R1, j);
        }
      }
    };

    template <std::size_t... Rs>
    auto constexpr slice() const noexcept
    {
      return slice_view<Rs...>(this);
    }

    auto constexpr operator [](std::size_t i) noexcept -> value_type &
    {
      assert(i < Row);
      return data[i];
    }

    auto constexpr operator [](std::size_t i) const noexcept -> value_type const&
    {
      assert(i < Row);
      return data[i];
    }

    auto constexpr begin() noexcept
    {
      return data.begin();
    }

    auto constexpr begin() const noexcept
    {
      return data.begin();
    }

    auto constexpr end() noexcept
    {
      return data.end();
    }

    auto constexpr end() const noexcept
    {
      return data.end();
    }

    auto constexpr size() const noexcept -> std::size_t
    {
      return data.size();
    }

    auto constexpr operator ()(std::size_t i, std::size_t j) noexcept -> bit_proxy<std::uint64_t>
    {
      assert(i < Row);
      assert(j < Column);
      auto const [q, r] = qr(j, 64);
      return { (*this)[i][q], std::uint64_t(1) << r };
    }

    auto constexpr operator ()(std::size_t i, std::size_t j) const noexcept -> bit_proxy<std::uint64_t const>
    {
      assert(i < Row);
      assert(j < Column);
      auto const [q, r] = qr(j, 64);
      return { (*this)[i][q], std::uint64_t(1) << r };
    }

    auto constexpr operator ()(std::size_t i, std::size_t first, std::size_t last) noexcept -> bit_range_proxy<std::uint64_t>
    {
      assert(i < Row);
      assert(first < last);
      assert(last <= Column);

      auto const [q1, r1] = qr(first, 64);
      auto const [q2, r2] = qr(last - 1, 64);

      return { std::span((*this)[i]).subspan(q1, q2 - q1 + 1), ~std::uint64_t(0) << r1, ~std::uint64_t(0) >> (63 - r2) };
    }

    auto constexpr operator ()(std::size_t i, std::size_t first, std::size_t last) const noexcept -> bit_range_proxy<std::uint64_t const>
    {
      assert(i < Row);
      assert(first < last);
      assert(last <= Column);

      auto const [q1, r1] = qr(first, 64);
      auto const [q2, r2] = qr(last - 1, 64);

      return { std::span((*this)[i]).subspan(q1, q2 - q1 + 1), ~std::uint64_t(0) << r1, ~std::uint64_t(0) >> (63 - r2) };
    }
  };
} // namespace meevax::memory

#endif // INCLUDED_MEEVAX_MEMORY_BIT_MATRIX_HPP
