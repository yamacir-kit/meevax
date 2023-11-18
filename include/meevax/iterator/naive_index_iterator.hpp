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

#ifndef INCLUDED_MEEVAX_ITERATOR_NAIVE_INDEX_ITERATOR_HPP
#define INCLUDED_MEEVAX_ITERATOR_NAIVE_INDEX_ITERATOR_HPP

#include <functional> // reference_wrapper
#include <iterator>
#include <limits>
#include <memory>
#include <type_traits>

namespace meevax
{
inline namespace iterator
{
  template <typename Container>
  struct naive_index_iterator
  {
    using iterator_category = std::bidirectional_iterator_tag;

    using value_type = std::decay_t<decltype(std::declval<Container>()[std::declval<std::size_t>()])>;

    using reference = decltype(std::declval<Container>()[std::declval<std::size_t>()]);

    using pointer = std::add_pointer_t<value_type>;

    using difference_type = std::ptrdiff_t;

    std::reference_wrapper<Container const> container;

    std::size_t index;

    explicit naive_index_iterator(Container const& container)
      : container { std::cref(container) }
      , index { container.size() }
    {}

    explicit naive_index_iterator(Container const& container, std::size_t index)
      : container { std::cref(container) }
      , index { index }
    {
      if (container.size() < index)
      {
        index = container.size();
      }
    }

    auto operator *() const -> decltype(auto)
    {
      assert(index < container.get().size());
      return container.get()[index];
    }

    auto operator ++() -> decltype(auto)
    {
      /*
         NOTE: Incrementing the end iterator is undefined behavior, so there is
         no need to consider that case.
      */
      assert(index < container.get().size());

      ++index;

      assert(index <= container.get().size());

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

      assert(index < container.get().size());

      return *this;
    }

    friend auto operator ==(naive_index_iterator const& a, naive_index_iterator const& b)
    {
      /*
         NOTE: Comparing iterators obtained from different containers is
         undefined behavior. Therefore, here, it may be assumed that
         a.container and b.container refer to the same container, meaning that
         they have the same address.
      */
      return a.index == b.index;
    }

    friend auto operator !=(naive_index_iterator const& a, naive_index_iterator const& b)
    {
      return not (a == b);
    }
  };
} // namespace iterator
} // namespace meevax

#endif // INCLUDED_MEEVAX_ITERATOR_NAIVE_INDEX_ITERATOR_HPP
