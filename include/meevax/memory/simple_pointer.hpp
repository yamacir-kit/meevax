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

#ifndef INCLUDED_MEEVAX_MEMORY_SIMPLE_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_SIMPLE_POINTER_HPP

#include <cstddef>
#include <memory>
#include <utility>

namespace meevax::inline memory
{
  template <typename T>
  struct simple_pointer
  {
    using element_type = T;

    using pointer = std::add_pointer_t<element_type>;

    pointer data;

    template <typename P = pointer>
    constexpr simple_pointer(typename std::pointer_traits<P>::pointer data = nullptr)
      : data { static_cast<pointer>(data) }
    {}

    constexpr simple_pointer(simple_pointer const& sp) = default;

    constexpr auto operator ->() const noexcept
    {
      return data;
    }

    constexpr auto operator *() const noexcept -> decltype(auto)
    {
      return *data;
    }

    constexpr explicit operator bool() const noexcept
    {
      return data != nullptr;
    }

    constexpr auto get() const noexcept
    {
      return data;
    }

    auto reset(pointer const p = nullptr) noexcept
    {
      data = p;
    }
  };

  template <typename T, typename U>
  constexpr auto operator ==(simple_pointer<T> const& x, simple_pointer<U> const& y)
  {
    return x.get() == y.get();
  }

  template <typename T, typename U>
  constexpr auto operator !=(simple_pointer<T> const& x, simple_pointer<U> const& y)
  {
    return x.get() != y.get();
  }
} // namespace meevax::memory

template <typename T>
struct std::hash<meevax::memory::simple_pointer<T>>
  : public std::hash<typename meevax::memory::simple_pointer<T>::pointer>
{};

#endif // INCLUDED_MEEVAX_MEMORY_SIMPLE_POINTER_HPP
