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

#ifndef INCLUDED_MEEVAX_MEMORY_TRIE_HPP
#define INCLUDED_MEEVAX_MEMORY_TRIE_HPP

#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <limits>
#include <meevax/memory/qr.hpp>
#include <memory>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>

namespace meevax::inline memory
{
  template <typename T, std::size_t... Ns>
  struct trie;

  template <typename T, std::size_t N, std::size_t... Ns>
  struct trie<T, N, Ns...>
  {
    static_assert(0 < N and ((0 < Ns) and ...));

    static_assert(N % 64 == 0 and ((Ns % 64 == 0) and ...));

    using value_type = T;

    using chunk = std::conditional_t<0 < sizeof...(Ns), trie<T, Ns...>, std::array<T, 1>>;

    std::array<chunk *, N> data {};

    std::array<std::uint64_t, N / 64> occupancy {};

    std::size_t i_min = N;

    std::size_t i_max = 0;

    auto static constexpr I = (static_cast<std::uintptr_t>(N) * ... * Ns);

    struct iterator
    {
      using value_type = typename trie::value_type;

      using reference = value_type &;

      using pointer = value_type *;

      using difference_type = std::ptrdiff_t;

      using iterator_concept = std::forward_iterator_tag;

      trie const* t = nullptr;

      std::uintptr_t i = I;

      auto constexpr operator *() const noexcept -> reference
      {
        assert(t);
        return t->at(i);
      }

      auto constexpr operator ->() const noexcept -> pointer
      {
        return std::addressof(**this);
      }

      auto constexpr operator ++() noexcept -> iterator &
      {
        assert(i < I);
        assert(t);
        return *this = t->lower_bound(i + 1);
      }

      auto constexpr operator ++(int) noexcept -> iterator
      {
        return std::exchange(*this, std::next(*this));
      }

      auto constexpr friend operator ==(iterator const& a, iterator const& b) noexcept -> bool
      {
        return a.t == b.t and a.i == b.i;
      }

      auto constexpr friend operator ==(iterator const& a, std::default_sentinel_t) noexcept -> bool
      {
        return a.i == I;
      }
    };

    constexpr trie() noexcept = default;

    trie(trie const&) = delete;

    auto operator =(trie const&) -> trie & = delete;

    auto constexpr at(std::uintptr_t i) const noexcept -> T &
    {
      assert(i < I);

      if constexpr (0 < sizeof...(Ns))
      {
        auto const [q, r] = qr(i, chunk::I);
        assert(data[q]);
        return data[q]->at(r);
      }
      else
      {
        assert(data[i]);
        return data[i]->front();
      }
    }

    auto constexpr operator [](std::uintptr_t i) -> T &
    {
      assert(i < I);

      auto touch = [this](std::size_t i) -> auto &
      {
        if (not data[i])
        {
          data[i] = new chunk();

          auto const [q, r] = qr(i, 64);

          occupancy[q] |= std::uint64_t(1) << r;

          i_min = std::min(i_min, i);
          i_max = std::max(i_max, i);
        }

        return *data[i];
      };

      if constexpr (0 < sizeof...(Ns))
      {
        auto const [q, r] = qr(i, chunk::I);
        return touch(q)[r];
      }
      else
      {
        return touch(i).front();
      }
    }

    auto constexpr begin() const noexcept -> iterator
    {
      if constexpr (0 < sizeof...(Ns))
      {
        return i_min == N ? iterator { this }
                          : iterator { this, i_min * chunk::I + data[i_min]->begin().i };
      }
      else
      {
        return { this, i_min };
      }
    }

    auto constexpr end() const noexcept -> std::default_sentinel_t
    {
      return {};
    }

    auto constexpr find(std::uintptr_t i) const noexcept -> iterator
    {
      assert(i < I);

      if constexpr (0 < sizeof...(Ns))
      {
        if (auto const [q, r] = qr(i, chunk::I); data[q] and data[q]->find(r) != std::default_sentinel)
        {
          return { this, i };
        }
        else
        {
          return { this };
        }
      }
      else
      {
        return data[i] ? iterator { this, i }
                       : iterator { this };
      }
    }

    auto constexpr lower_bound(std::uintptr_t i) const noexcept -> iterator
    {
      assert(i <= I);

      auto seek = [this](std::size_t i)
      {
        if (i = std::max(i, i_min); i_max < i)
        {
          return N;
        }
        else
        {
          auto [q, r] = qr(i, 64);

          auto w = occupancy[q] & (~std::uint64_t(0) << r);

          while (not w)
          {
            assert(q + 1 < N / 64);
            w = occupancy[++q];
          }

          return q * 64 + std::countr_zero(w);
        }
      };

      if constexpr (0 < sizeof...(Ns))
      {
        auto [q, r] = qr(i, chunk::I);

        if (q < N and data[q])
        {
          if (auto const j = data[q]->lower_bound(r).i; j < chunk::I)
          {
            return { this, q * chunk::I + j };
          }
        }

        q = seek(q + 1);

        return q == N ? iterator { this }
                      : iterator { this, q * chunk::I + data[q]->begin().i };
      }
      else
      {
        return { this, seek(i) };
      }
    }

    auto constexpr swap(trie & other) noexcept -> void
    {
      data.swap(other.data);
      occupancy.swap(other.occupancy);
      std::swap(i_min, other.i_min);
      std::swap(i_max, other.i_max);
    }

    auto constexpr clear() noexcept -> void
    {
      assert(i_min == N or i_min <= i_max);
      assert(i_max < N);

      for (auto q = i_min / 64; q <= i_max / 64; ++q)
      {
        for (auto w = occupancy[q]; w; w &= w - 1)
        {
          auto & p = data[q * 64 + std::countr_zero(w)];

          assert(p);

          if constexpr (0 < sizeof...(Ns))
          {
            p->clear();
          }

          delete std::exchange(p, nullptr);
        }

        occupancy[q] = 0;
      }

      i_min = N;
      i_max = 0;
    }
  };
} // namespace meevax::memory

#endif // INCLUDED_MEEVAX_MEMORY_TRIE_HPP
