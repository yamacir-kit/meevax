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

#ifndef INCLUDED_MEEVAX_MEMORY_NAN_BOXING_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_NAN_BOXING_POINTER_HPP

#include <stdexcept>

#include <meevax/memory/simple_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct nan_boxing_pointer : public simple_pointer<T>
  {
    using pointer = typename simple_pointer<T>::pointer;

    static constexpr std::uintptr_t mask_sign         = 0b1000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t mask_exponent     = 0b0111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t mask_fraction     = 0b0000'0000'0000'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111;
    static constexpr std::uintptr_t mask_quiet        = 0b0000'0000'0000'1000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t mask_type         = 0b0000'0000'0000'0111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;

    static constexpr std::uintptr_t mask_signature    = 0b0111'1111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t mask_payload      = 0b0000'0000'0000'0000'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111;

    static constexpr std::uintptr_t signature_pointer = 0b0111'1111'1111'1001'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b001 = 0b0111'1111'1111'1010'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;

    template <typename P = pointer>
    constexpr nan_boxing_pointer(typename std::pointer_traits<P>::pointer data = nullptr)
      : simple_pointer<T> {
          reinterpret_cast<pointer>(
            signature_pointer | reinterpret_cast<std::uintptr_t>(data)) }
    {}

    // constexpr explicit nan_boxing_pointer(std::uint32_t const value)
    //   : simple_pointer<T> {
    //       reinterpret_cast<pointer>(
    //         exponent | quiet | is_uint32_t | value) }
    // {}

    constexpr auto operator *() const -> decltype(auto)
    {
      return *operator ->();
    }

    constexpr auto operator ->() const
    {
      switch (signature())
      {
      case signature_pointer:
        return reinterpret_cast<pointer>(reinterpret_cast<std::uintptr_t>(simple_pointer<T>::data) & mask_payload);

      default:
        throw std::logic_error("");
      }
    }

    template <typename U>
    auto is() const noexcept
    {
      return type() == typeid(typename std::decay<U>::type);
    }

    constexpr auto signature() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(simple_pointer<T>::data) & mask_signature;
    }

    constexpr auto type() const noexcept -> decltype(auto)
    {
      switch (signature())
      {
      case signature_pointer:
        return typeid(pointer);

      default:
        return typeid(void);
      }
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_NAN_BOXING_POINTER_HPP

