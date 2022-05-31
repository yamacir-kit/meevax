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

#include <memory>
#include <type_traits>
#include <typeinfo>

#include <meevax/memory/bit_cast.hpp>
#include <meevax/type_traits/integer.hpp>

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
  struct nan_boxing_pointer
  {
    using element_type = typename std::decay<T>::type;

    using pointer = typename std::add_pointer<element_type>::type;

    pointer data;

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

    constexpr nan_boxing_pointer(nan_boxing_pointer const&) = default;

    template <typename P = pointer>
    constexpr nan_boxing_pointer(typename std::pointer_traits<P>::pointer data = nullptr)
      : data { reinterpret_cast<pointer>(signature_pointer | reinterpret_cast<std::uintptr_t>(data)) }
    {}

    constexpr nan_boxing_pointer(double const& value)
      : data { reinterpret_cast<pointer>(bit_cast<uintN_t<sizeof(double)>>(value)) }
    {}

    auto operator =(double const& value) -> auto &
    {
      data = reinterpret_cast<pointer>(bit_cast<uintN_t<sizeof(double)>>(value));
      return *this;
    }

    #define DEFINE(TYPE)                                                       \
    constexpr nan_boxing_pointer(TYPE const& value)                            \
      : data { reinterpret_cast<pointer>(                                      \
                 signature_##TYPE | bit_cast<uintN_t<sizeof(TYPE)>>(value)) }  \
    {}                                                                         \
                                                                               \
    auto operator =(TYPE const& value) -> auto &                               \
    {                                                                          \
      data = reinterpret_cast<pointer>(                                        \
               signature_##TYPE | bit_cast<uintN_t<sizeof(TYPE)>>(value));     \
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

    constexpr auto operator ->() const
    {
      return get();
    }

    constexpr auto operator *() const -> decltype(auto)
    {
      return *get();
    }

    constexpr explicit operator bool() const noexcept
    {
      return get() != nullptr;
    }

    template <typename U>
    auto as() const
    {
      if constexpr (std::is_same<double, typename std::decay<U>::type>::value)
      {
        return bit_cast<double>(data);
      }
      else
      {
        return bit_cast<typename std::decay<U>::type>(
                 static_cast<uintN_t<sizeof(typename std::decay<U>::type)>>(
                   reinterpret_cast<std::uintptr_t>(data) & mask_payload));
      }
    }

    constexpr auto get() const noexcept -> pointer
    {
      switch (signature())
      {
      case signature_pointer:
        return reinterpret_cast<pointer>(reinterpret_cast<std::uintptr_t>(data) & mask_payload);

      default:
        return nullptr;
      }
    }

    template <typename U>
    auto is() const noexcept
    {
      return type() == typeid(typename std::decay<U>::type);
    }

    auto reset(pointer const p = nullptr) noexcept
    {
      data = reinterpret_cast<pointer>(signature_pointer | reinterpret_cast<std::uintptr_t>(p));
    }

    constexpr auto signature() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(data) & mask_signature;
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

  template <typename... Ts>
  constexpr auto operator ==(nan_boxing_pointer<Ts...> const& x,
                             nan_boxing_pointer<Ts...> const& y)
  {
    return x.data == y.data;
  }

  template <typename... Ts>
  constexpr auto operator !=(nan_boxing_pointer<Ts...> const& x,
                             nan_boxing_pointer<Ts...> const& y)
  {
    return x.data != y.data;
  }
} // namespace memory
} // namespace meevax

namespace std
{
  template <typename... Ts>
  class hash<meevax::memory::nan_boxing_pointer<Ts...>>
    : public hash<typename meevax::memory::nan_boxing_pointer<Ts...>::pointer>
  {};
}

#endif // INCLUDED_MEEVAX_MEMORY_NAN_BOXING_POINTER_HPP

