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

#ifndef INCLUDED_MEEVAX_MEMORY_SIMPLE_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_SIMPLE_POINTER_HPP

#include <cassert>
#include <cstddef>
#include <memory>
#include <utility>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct simple_pointer
  {
    using value_type = T;

    using reference = typename std::add_lvalue_reference<value_type>::type;

    using const_reference = typename std::add_const<reference>::type;

    using pointer = value_type *;

    using const_pointer = value_type const*;

    pointer data;

    template <typename Pointer = pointer>
    constexpr simple_pointer(typename std::pointer_traits<Pointer>::pointer data = nullptr)
      : data { static_cast<pointer>(data) }
    {}

    constexpr simple_pointer(simple_pointer const& sp)
      : data { sp.get() }
    {}

    template <typename... Ts>
    auto operator =(Ts&&... xs) noexcept -> decltype(auto)
    {
      return store(std::forward<decltype(xs)>(xs)...);
    }

    auto operator ->() const noexcept
    {
      return get();
    }

    auto operator *() const noexcept -> const_reference
    {
      return load();
    }

    auto operator *() noexcept -> reference
    {
      return load();
    }

    explicit constexpr operator bool() const noexcept
    {
      return data != nullptr;
    }

    constexpr auto get() const noexcept -> pointer
    {
      return data;
    }

    constexpr auto load() const noexcept -> const_reference
    {
      assert(data);
      return *data;
    }

    constexpr auto load() noexcept -> reference
    {
      assert(data);
      return *data;
    }

    auto reset(pointer const p = nullptr) noexcept -> pointer
    {
      return data = p;
    }

    auto store(simple_pointer const& x) noexcept -> auto &
    {
      data = x.get();
      return *this;
    }
  };

  template <typename T, typename U>
  constexpr auto operator ==(simple_pointer<T> const& x,
                             simple_pointer<U> const& y)
  {
    return x.get() == y.get();
  }

  template <typename T, typename U>
  constexpr auto operator !=(simple_pointer<T> const& x,
                             simple_pointer<U> const& y)
  {
    return x.get() != y.get();
  }
} // namespace memory
} // namespace meevax

namespace std
{
  template <typename T>
  class hash<meevax::memory::simple_pointer<T>>
    : public hash<typename meevax::memory::simple_pointer<T>::pointer>
  {};
}

#endif // INCLUDED_MEEVAX_MEMORY_SIMPLE_POINTER_HPP
