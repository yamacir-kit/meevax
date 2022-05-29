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

#include <cstring>
#include <stdexcept>

#include <meevax/memory/simple_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T,
            typename T_0b010 = std::integral_constant<std::uint32_t, 0b010>,
            typename T_0b011 = std::integral_constant<std::uint32_t, 0b011>,
            typename T_0b100 = std::integral_constant<std::uint32_t, 0b100>,
            typename T_0b101 = std::integral_constant<std::uint32_t, 0b101>,
            typename T_0b110 = std::integral_constant<std::uint32_t, 0b110>,
            typename T_0b111 = std::integral_constant<std::uint32_t, 0b111>>
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
    static constexpr std::uintptr_t signature_T_0b010 = 0b0111'1111'1111'1010'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b011 = 0b0111'1111'1111'1011'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b100 = 0b0111'1111'1111'1100'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b101 = 0b0111'1111'1111'1101'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b110 = 0b0111'1111'1111'1110'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b111 = 0b0111'1111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;

    template <typename P = pointer>
    constexpr nan_boxing_pointer(typename std::pointer_traits<P>::pointer data = nullptr)
      : simple_pointer<T> {
          reinterpret_cast<pointer>(
            signature_pointer | reinterpret_cast<std::uintptr_t>(data)) }
    {
      assert((reinterpret_cast<std::uintptr_t>(data) & ~mask_payload) == 0);
    }

    explicit constexpr nan_boxing_pointer(double const& value)
      : simple_pointer<T> {
          reinterpret_cast<pointer>(
            *reinterpret_cast<std::uintptr_t const*>(&value)) }
    {}

    auto operator =(double const& value) -> auto &
    {
      simple_pointer<T>::data
        = reinterpret_cast<pointer>(
            *reinterpret_cast<std::uintptr_t const*>(&value));
      return *this;
    }

    #define DEFINE(TYPE)                                                       \
    explicit constexpr nan_boxing_pointer(TYPE const& value)                   \
      : simple_pointer<T> {                                                    \
          reinterpret_cast<pointer>(                                           \
            signature_##TYPE | *reinterpret_cast<std::uint32_t const*>(&value)) } \
    {}                                                                         \
                                                                               \
    auto operator =(TYPE const& value) -> auto &                               \
    {                                                                          \
      simple_pointer<T>::data                                                  \
        = reinterpret_cast<pointer>(                                           \
            signature_##TYPE | *reinterpret_cast<std::uint32_t const*>(&value)); \
      return *this;                                                            \
    }                                                                          \
                                                                               \
    static_assert(sizeof(TYPE) <= 4)

    DEFINE(T_0b010);
    DEFINE(T_0b011);
    DEFINE(T_0b100);
    DEFINE(T_0b101);
    DEFINE(T_0b110);
    DEFINE(T_0b111);

    #undef DEFINE

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
    auto as() const
    {
      if constexpr (std::is_same<double, typename std::decay<U>::type>::value)
      {
        const auto from = reinterpret_cast<std::uintptr_t>(simple_pointer<T>::data);
        double to;
        std::memcpy(&to, &from, sizeof(double));
        return to;
      }
      else
      {
        const auto from = reinterpret_cast<std::uintptr_t>(simple_pointer<T>::data) & mask_payload;
        typename std::decay<U>::type to;
        std::memcpy(&to, &from, sizeof(typename std::decay<U>::type));
        return to;
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

      #define DEFINE(TYPE)                                                     \
      case signature_##TYPE:                                                   \
        return typeid(typename std::decay<TYPE>::type)

      DEFINE(T_0b010);
      DEFINE(T_0b011);
      DEFINE(T_0b100);
      DEFINE(T_0b101);
      DEFINE(T_0b110);
      DEFINE(T_0b111);

      #undef DEFINE

      default:
        return typeid(double);
      }
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_NAN_BOXING_POINTER_HPP

