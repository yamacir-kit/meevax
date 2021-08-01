/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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

#include <functional>
#include <iterator> // std::begin, std::end, std::distance

#include <meevax/kernel/pair.hpp>
#include <meevax/utility/unwrap_reference_wrapper.hpp>

namespace meevax
{
inline namespace kernel
{
  struct homoiconic_iterator
    : public std::reference_wrapper<let const>
  {
    using iterator_category = std::forward_iterator_tag;

    using value_type = let;

    using reference = let &;

    using const_reference = let const&;

    using pointer = const_reference; // homoiconicity

    using difference_type = std::ptrdiff_t;

    using size_type = std::size_t;

    homoiconic_iterator(let const&);

    auto operator *() const -> const_reference;

    auto operator ->() const -> pointer;

    auto operator ++() -> homoiconic_iterator &;

    auto operator ++(int) -> homoiconic_iterator;

    explicit operator bool() const;

    auto unwrap() const noexcept -> const_reference;
  };

  auto operator ==(homoiconic_iterator const& lhs, homoiconic_iterator const& rhs) noexcept -> bool;

  auto operator !=(homoiconic_iterator const& lhs, homoiconic_iterator const& rhs) noexcept -> bool;
} // namespace kernel
} // namespace meevax

namespace std
{
  auto begin(meevax::let const&) -> meevax::homoiconic_iterator;

  auto cbegin(meevax::let const&) -> meevax::homoiconic_iterator;

  auto cend(meevax::let const&) -> meevax::homoiconic_iterator const&;

  auto end(meevax::let const&) -> meevax::homoiconic_iterator const&;
} // namespace std

#endif // INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP
