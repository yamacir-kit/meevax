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

#ifndef INCLUDED_MEEVAX_RANGES_SET_BITS_VIEW_HPP
#define INCLUDED_MEEVAX_RANGES_SET_BITS_VIEW_HPP

#include <bit>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <ranges>

namespace meevax::inline ranges
{
  template <typename T>
  struct set_bits_view : std::ranges::view_interface<set_bits_view<T>>
  {
    struct iterator
    {
      using value_type = std::size_t;

      using difference_type = std::ptrdiff_t;

      using iterator_concept = std::forward_iterator_tag;

      std::ranges::iterator_t<T const> current {};

      std::ranges::iterator_t<T const> end {};

      std::size_t offset = 0;

      std::uint64_t remaining = 0;

      iterator() = default;

      iterator(std::ranges::iterator_t<T const> current, std::ranges::iterator_t<T const> end)
        : current { current }
        , end { end }
      {
        seek();
      }

      auto seek() -> void
      {
        while (current != end)
        {
          if ((remaining = *current))
          {
            break;
          }
          else
          {
            ++current;
            offset += 64;
          }
        }
      }

      auto operator *() const -> value_type
      {
        return offset + std::countr_zero(remaining);
      }

      auto operator ++() -> iterator &
      {
        remaining &= remaining - 1;

        if (not remaining)
        {
          ++current;
          offset += 64;
          seek();
        }

        return *this;
      }

      auto operator ++(int) -> iterator
      {
        auto previous = *this;
        ++*this;
        return previous;
      }

      friend auto operator ==(iterator const&, iterator const&) -> bool = default;

      friend auto operator ==(iterator const& i, std::default_sentinel_t) -> bool
      {
        return i.current == i.end;
      }
    };

    T words;

    constexpr set_bits_view() = default;

    explicit set_bits_view(T words)
      : words { words }
    {}

    auto begin() const -> iterator
    {
      return iterator(std::ranges::begin(words), std::ranges::end(words));
    }

    auto end() const -> std::default_sentinel_t
    {
      return {};
    }
  };
} // namespace meevax::ranges

#endif // INCLUDED_MEEVAX_RANGES_SET_BITS_VIEW_HPP
