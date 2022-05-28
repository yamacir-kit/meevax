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

#ifndef INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP

#include <cstddef>
#include <meevax/memory/simple_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct tagged_pointer : public simple_pointer<T>
  {
    using pointer = typename simple_pointer<T>::pointer;

    static_assert(8 <= sizeof(pointer));

    using simple_pointer<T>::simple_pointer;

    constexpr auto operator *() const -> decltype(auto)
    {
      switch (tag())
      {
      case 0b000:
        return simple_pointer<T>::operator *();

      default:
        throw std::logic_error("");
      }
    }

    template <typename U>
    auto is() const noexcept
    {
      return type() == typeid(typename std::decay<U>::type);
    }

    constexpr auto tag() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(simple_pointer<T>::get()) & 0b111;
    }

    constexpr auto type() const noexcept -> decltype(auto)
    {
      switch (tag())
      {
      case 0b000:
        return simple_pointer<T>::operator bool() ? typeid(pointer) : typeid(std::nullptr_t);

      default:
        return typeid(void);
      }
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP
