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

#include <iterator> // std::begin, std::end, std::distance

#include <meevax/kernel/pair.hpp>

#define MEEVAX_ITERATOR_USE_REFERENCE_WRAPPER

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct homoiconic_iterator
  #ifdef MEEVAX_ITERATOR_USE_REFERENCE_WRAPPER
    : public std::reference_wrapper<T>
  #else
    : public let
  #endif
  {
    using iterator_category = std::forward_iterator_tag;

    #ifdef MEEVAX_ITERATOR_USE_REFERENCE_WRAPPER
    using value_type = std::reference_wrapper<T>;
    #else
    using value_type = let;
    #endif

    using reference = typename std::add_lvalue_reference<value_type>::type;

    using const_reference = typename std::add_const<reference>::type;

    using pointer = value_type; // homoiconicity

    using difference_type = std::ptrdiff_t;

    using size_type = std::size_t;

  public:
    using value_type::value_type;

    #ifdef MEEVAX_ITERATOR_USE_REFERENCE_WRAPPER
    homoiconic_iterator(T const& x)
      : std::reference_wrapper<T> { std::cref(x) }
    {}
    #else
    template <typename... Ts>
    constexpr homoiconic_iterator(Ts&&... xs)
      : let { std::forward<decltype(xs)>(xs)... }
    {}
    #endif

    auto operator *() const -> let const&
    {
      return car(*this);
    }

    auto operator ->() const -> let const&
    {
      return operator *();
    }

    auto operator ++() -> auto &
    {
      return *this = cdr(*this);
    }

    auto operator ++(int)
    {
      auto copy = *this;
      operator++();
      return copy;
    }

    #ifdef MEEVAX_ITERATOR_USE_REFERENCE_WRAPPER
    auto operator ==(homoiconic_iterator const& rhs) const noexcept -> decltype(auto)
    {
      return get() == rhs.get();
    }

    auto operator !=(homoiconic_iterator const& rhs) const noexcept -> decltype(auto)
    {
      return get() != rhs.get();
    }

    operator bool() const
    {
      return static_cast<bool>(static_cast<T const&>(*this));
    }

    operator T&() const noexcept
    {
      return std::reference_wrapper<T>::get();
    }

    using std::reference_wrapper<T>::get;
    #endif

    auto cbegin() const noexcept -> homoiconic_iterator { return *this; }
    auto  begin() const noexcept -> homoiconic_iterator { return *this; }
    auto   cend() const noexcept -> homoiconic_iterator { return unit; }
    auto    end() const noexcept -> homoiconic_iterator { return unit; }
  };
} // namespace kernel
} // namespace meevax

namespace std
{
  auto cbegin(meevax::let const& x) -> meevax::homoiconic_iterator<meevax::let const>;
  auto  begin(meevax::let const& x) -> meevax::homoiconic_iterator<meevax::let const>;
  auto   cend(meevax::let const&  ) -> meevax::homoiconic_iterator<meevax::let const>;
  auto    end(meevax::let const&  ) -> meevax::homoiconic_iterator<meevax::let const>;
} // namespace std

#endif // INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP
