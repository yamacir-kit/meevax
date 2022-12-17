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

#ifndef INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct iterator : public std::reference_wrapper<const object>
  {
    using iterator_category = std::forward_iterator_tag;

    using value_type = object;

    using reference = std::add_lvalue_reference_t<value_type>;

    using const_reference = std::add_const_t<reference>;

    using pointer = std::add_pointer_t<value_type>;

    using difference_type = std::ptrdiff_t;

    using size_type = std::size_t;

    iterator(object const& x)
      : std::reference_wrapper<const object> { std::cref(x) }
    {}

    auto operator *() const -> const_reference;

    auto operator ->() const -> pointer;

    auto operator ++() -> iterator &;

    auto operator ++(int) -> iterator;

    operator bool() const
    {
      return static_cast<bool>(get());
    }
  };

  auto operator ==(iterator const&, iterator const&) noexcept -> bool;

  auto operator !=(iterator const&, iterator const&) noexcept -> bool;
} // namespace kernel
} // namespace meevax

namespace std
{
  auto begin(meevax::object const&) -> meevax::iterator;

  auto cbegin(meevax::object const&) -> meevax::iterator;

  auto cend(meevax::object const&) -> meevax::iterator const&;

  auto end(meevax::object const&) -> meevax::iterator const&;
} // namespace std

#endif // INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP
