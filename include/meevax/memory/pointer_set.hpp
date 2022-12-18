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

#ifndef INCLUDED_MEEVAX_MEMORY_POINTER_SET_HPP
#define INCLUDED_MEEVAX_MEMORY_POINTER_SET_HPP

#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <iterator>
#include <limits>
#include <vector>

namespace meevax
{
inline namespace memory
{
  template <typename T, auto N = 1024 /* NOTE: This number has no technical basis and is probably not efficient */>
  class pointer_set
  {
    static_assert(std::is_pointer_v<T>);

    static constexpr auto word_capacity = std::numeric_limits<std::uint64_t>::digits;

    static constexpr auto page_capacity = word_capacity * N;

    static constexpr auto page_number_of(std::uint64_t value)
    {
      return value / page_capacity;
    }

    static constexpr auto word_number_of(std::uint64_t value)
    {
      return (value - page_number_of(value) * page_capacity) / word_capacity;
    }

    static constexpr auto char_number_of(std::uint64_t value)
    {
      return value - page_number_of(value) * page_capacity
                   - word_number_of(value) * word_capacity;
    }

    static constexpr auto signature_of(std::uint64_t value)
    {
      return static_cast<std::uint64_t>(1) << char_number_of(value);
    }

    static constexpr auto compress(T value) -> std::uint64_t
    {
      return reinterpret_cast<std::uintptr_t>(value) >> 3;
    }

    static constexpr auto decompress(std::uint64_t value)
    {
      return reinterpret_cast<T>(value << 3);
    }

    struct page
    {
      std::size_t number;

      std::array<std::uint64_t, N> words;

      explicit constexpr page(std::size_t number)
        : number { number }
        , words {}
      {}

      constexpr auto word_of(std::uint64_t value) -> auto &
      {
        return words[word_number_of(value)];
      }

      constexpr auto operator <(std::size_t other_page_number)
      {
        return number < other_page_number;
      }
    };

    std::vector<page> pages;

  public:
    struct iterator
    {
      using iterator_category = std::forward_iterator_tag;

      using value_type = T;

      using reference = std::add_lvalue_reference_t<value_type>;

      using pointer = std::add_pointer_t<value_type>;

      using difference_type = std::ptrdiff_t;

      page const* pages;

                       std::size_t page_index_max;
      static constexpr std::size_t word_index_max = N;
      static constexpr std::size_t char_index_max = word_capacity;

      std::size_t page_index,
                  word_index,
                  char_index;

      explicit iterator(std::vector<page> const& pages,
                        std::size_t page_index,
                        std::size_t word_index,
                        std::size_t char_index)
        : pages          { pages.data() }
        , page_index_max { pages.size() }
        , page_index { page_index }
        , word_index { word_index }
        , char_index { char_index }
      {
        if (not pointing_to_an_inserted_element())
        {
          operator ++();
        }
      }

      explicit iterator(std::vector<page> const& pages)
        : pages          { pages.data() }
        , page_index_max { pages.size() }
        , page_index { std::numeric_limits<std::size_t>::max() }
        , word_index { std::numeric_limits<std::size_t>::max() }
        , char_index { std::numeric_limits<std::size_t>::max() }
      {}

      auto pointing_to_an_inserted_element() const -> bool
      {
        return page_index < page_index_max and
               word_index < word_index_max and
               char_index < char_index_max and
               pages[page_index].words[word_index] & (std::uint64_t(1) << char_index);
      }

      auto operator *() const
      {
        return decompress(pages[page_index].number * page_capacity + word_index * word_capacity + char_index);
      }

      auto operator ++() -> iterator &
      {
        ++char_index;

        for (; page_index < page_index_max; ++page_index, word_index = 0)
        {
          for (auto&& page = pages[page_index]; word_index < word_index_max; ++word_index, char_index = 0)
          {
            if (auto&& word = page.words[word_index]; word)
            {
              for (; char_index < char_index_max; ++char_index)
              {
                if (word & (std::uint64_t(1) << char_index))
                {
                  return *this;
                }
              }
            }
          }
        }

        page_index = std::numeric_limits<std::size_t>::max();
        word_index = std::numeric_limits<std::size_t>::max();
        char_index = std::numeric_limits<std::size_t>::max();

        return *this; // end
      }

      auto operator --() -> auto &
      {
        page_index = page_index_max <= page_index ? page_index_max - 1 : page_index;
        word_index = word_index_max <= word_index ? word_index_max - 1 : word_index;
        char_index = char_index_max <= char_index ? char_index_max - 1 : char_index - 1;

        /*
           NOTE: N4659 6.9.1.4

           Unsigned integers shall obey the laws of arithmetic modulo 2 n where
           n is the number of bits in the value representation of that
           particular size of integer.
        */

        for (; page_index < page_index_max; --page_index)
        {
          auto&& page = pages[page_index];

          for (; word_index < word_index_max; --word_index)
          {
            auto&& word = page.words[word_index];

            for (; char_index < char_index_max; --char_index)
            {
              if (word & (std::uint64_t(1) << char_index))
              {
                return *this;
              }
            }

            char_index = char_index_max - 1;
          }

          word_index = word_index_max - 1;
        }

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
        return page_index == rhs.page_index and
               word_index == rhs.word_index and
               char_index == rhs.char_index;
      }

      auto operator !=(iterator const& rhs)
      {
        return not operator ==(rhs);
      }
    };

    explicit pointer_set()
      : pages {}
    {
      assert(pages.size() == 0);
      assert(begin() == end());
    }

    auto lower_bound_page_of(std::uint64_t value)
    {
      if (auto target_page_number = page_number_of(value);
          0 < pages.size() and pages.front().number < target_page_number)
      {
        if (auto hint_index = target_page_number - pages.front().number;
            hint_index < pages.size())
        {
          if (auto iter = std::next(std::begin(pages), hint_index);
              iter->number == target_page_number)
          {
            return iter;
          }
          else
          {
            return std::lower_bound(std::begin(pages), std::end(pages), target_page_number);
          }
        }
        else
        {
          return std::lower_bound(std::begin(pages), std::end(pages), target_page_number);
        }
      }
      else
      {
        return std::begin(pages);
      }
    }

    auto page_count() const
    {
      return pages.size();
    }

    auto size() const
    {
      return std::distance(begin(), end());
    }

    auto insert(T value)
    {
      assert(decompress(compress(value)) == value);

      auto compressed = compress(value);

      if (auto iter = lower_bound_page_of(compressed);
          iter != std::end(pages) and iter->number == page_number_of(compressed))
      {
        iter->word_of(compressed) |= signature_of(compressed);
      }
      else
      {
        assert(iter == std::end(pages) or page_number_of(compressed) < iter->number);
        iter = pages.insert(iter, page(page_number_of(compressed)));
        iter->word_of(compressed) |= signature_of(compressed);
      }
    }

    auto erase(T value)
    {
      lower_bound_page_of(compress(value))->word_of(compress(value)) &= ~signature_of(compress(value));
    }

    auto erase(iterator iter)
    {
      erase(*iter);
      return ++iter;
    }

    auto density() const // for parameter tuning
    {
      double any_bit_count = pages.size() * page_capacity;
      double one_bit_count = size();

      return one_bit_count / any_bit_count;
    }

    auto begin() const
    {
      return iterator(pages, 0, 0, 0);
    }

    auto end() const noexcept
    {
      return iterator(pages);
    }

    auto lower_bound(T value)
    {
      auto compressed = compress(value);

      if (auto iter = lower_bound_page_of(compressed); iter != std::end(pages))
      {
        return iterator(pages,
                        std::distance(std::begin(pages), iter),
                        word_number_of(compressed),
                        char_number_of(compressed));
      }
      else
      {
        return end();
      }
    }

    template <typename... Ts>
    auto find(Ts&&... xs)
    {
      return std::find(begin(), end(), std::forward<decltype(xs)>(xs)...);
    }

    template <typename... Ts>
    auto contains(Ts&&... xs)
    {
      return std::binary_search(begin(), end(), std::forward<decltype(xs)>(xs)...);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_POINTER_SET_HPP
