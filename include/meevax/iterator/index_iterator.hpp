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

#ifndef INCLUDED_MEEVAX_ITERATOR_INDEX_ITERATOR_HPP
#define INCLUDED_MEEVAX_ITERATOR_INDEX_ITERATOR_HPP

#include <cassert>
#include <iterator>
#include <limits>
#include <memory>
#include <type_traits>

namespace meevax::inline iterator
{
  template <typename T>
  struct index_iterator
  {
    using iterator_category = std::bidirectional_iterator_tag;

    using value_type = std::decay_t<decltype(std::declval<T>()[std::declval<std::size_t>()])>;

    using reference = decltype(std::declval<T>()[std::declval<std::size_t>()]);

    using pointer = std::add_pointer_t<value_type>;

    using difference_type = std::ptrdiff_t;

    T const* data = nullptr;

    std::size_t index = std::numeric_limits<std::size_t>::max();

    constexpr index_iterator() = default;

    explicit index_iterator(T const& data, std::size_t index)
      : data { std::addressof(data) }
      , index { index }
    {}

    auto out_of_range() const -> bool
    {
      return not data or index == data->size();
    }

    auto operator *() const -> decltype(auto)
    {
      assert(data);
      assert(index < data->size());
      return (*data)[index];
    }

    auto operator ++() -> decltype(auto)
    {
      /*
         NOTE: Incrementing the end iterator is undefined behavior, so there is
         no need to consider that case.
      */
      assert(data);
      assert(index < data->size());

      ++index;

      assert(index <= data->size());

      return *this;
    }

    auto operator --() -> decltype(auto)
    {
      /*
         NOTE: Decrementing the begin iterator is undefined behavior, so there
         is no need to consider that case.
      */
      assert(index != 0);

      --index;

      assert(data);
      assert(index < data->size());

      return *this;
    }

    friend auto operator ==(index_iterator const& a, index_iterator const& b)
    {
      /*
         NOTE: Comparing iterators obtained from different containers is
         undefined behavior. Therefore, here, it may be assumed that
         a.container and b.container refer to the same container, meaning that
         they have the same address.
      */
      return a.index == b.index or (a.out_of_range() and b.out_of_range());
    }

    friend auto operator !=(index_iterator const& a, index_iterator const& b)
    {
      return not (a == b);
    }
  };
} // namespace meevax::iterator

#endif // INCLUDED_MEEVAX_ITERATOR_INDEX_ITERATOR_HPP
