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

#include <cstddef>
#include <iomanip>
#include <memory>
#include <ostream>
#include <type_traits>
#include <typeinfo>

#include <meevax/memory/bit_cast.hpp>
#include <meevax/type_traits/integer.hpp>

namespace meevax
{
inline namespace memory
{
  using float64 = double;

  static_assert(std::numeric_limits<float64>::is_iec559 and sizeof(float64) == 8);

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

    static constexpr std::uintptr_t signature_float64 = 0b0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_pointer = 0b0111'1111'1111'1001'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b010 = 0b0111'1111'1111'1010'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b011 = 0b0111'1111'1111'1011'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b100 = 0b0111'1111'1111'1100'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b101 = 0b0111'1111'1111'1101'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b110 = 0b0111'1111'1111'1110'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T_0b111 = 0b0111'1111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;

    explicit nan_boxing_pointer(std::nullptr_t = nullptr)
      : nan_boxing_pointer { static_cast<pointer>(nullptr) }
    {}

    auto reset(std::nullptr_t = nullptr)
    {
      reset(static_cast<pointer>(nullptr));
    }

    explicit nan_boxing_pointer(nan_boxing_pointer const&) = default;

    auto reset(nan_boxing_pointer const& value) -> void
    {
      data = value.data;
    }

    #define DEFINE(TYPE)                                                       \
    explicit nan_boxing_pointer(TYPE const& value) noexcept                    \
      : data { reinterpret_cast<pointer>(                                      \
                 signature_##TYPE | bit_cast<uintN_t<sizeof(TYPE)>>(value)) }  \
    {}                                                                         \
                                                                               \
    auto reset(TYPE const& value) noexcept -> void                             \
    {                                                                          \
      data = reinterpret_cast<pointer>(                                        \
               signature_##TYPE | bit_cast<uintN_t<sizeof(TYPE)>>(value));     \
    }                                                                          \
                                                                               \
    static_assert(std::is_same_v<TYPE, float64> or                             \
                  std::is_same_v<TYPE, pointer> or                             \
                  sizeof(TYPE) <= 4)

    DEFINE(float64); // 0b000
    DEFINE(pointer); // 0b001
    DEFINE(T_0b010);
    DEFINE(T_0b011);
    DEFINE(T_0b100);
    DEFINE(T_0b101);
    DEFINE(T_0b110);
    DEFINE(T_0b111);

    #undef DEFINE

    template <typename... Ts>
    auto operator =(Ts&&... xs) -> auto &
    {
      reset(std::forward<decltype(xs)>(xs)...);
      return *this;
    }

    auto operator ->() const
    {
      return get();
    }

    auto operator *() const -> decltype(auto)
    {
      return *get();
    }

    explicit operator bool() const noexcept
    {
      return get() != nullptr;
    }

    template <typename U>
    auto as() const
    {
      if constexpr (std::is_same<float64, typename std::decay<U>::type>::value)
      {
        return bit_cast<float64>(data);
      }
      else
      {
        return bit_cast<typename std::decay<U>::type>(
                 static_cast<uintN_t<sizeof(typename std::decay<U>::type)>>(
                   reinterpret_cast<std::uintptr_t>(data) & mask_payload));
      }
    }

    auto dereferenceable() const noexcept
    {
      return signature() == signature_pointer;
    }

    auto equivalent_to(nan_boxing_pointer const& nbp) const noexcept
    {
      return data == nbp.data;
    }

    auto get() const noexcept -> pointer
    {
      return dereferenceable() ? reinterpret_cast<pointer>(reinterpret_cast<std::uintptr_t>(data) & mask_payload) : nullptr;
    }

    template <typename U>
    auto is() const noexcept
    {
      return type() == typeid(typename std::decay<U>::type);
    }

    auto signature() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(data) & mask_signature;
    }

    auto type() const noexcept -> decltype(auto)
    {
      switch (signature())
      {
      #define DEFINE(TYPE)                                                     \
      case signature_##TYPE:                                                   \
        return typeid(TYPE)

      DEFINE(pointer);
      DEFINE(T_0b010);
      DEFINE(T_0b011);
      DEFINE(T_0b100);
      DEFINE(T_0b101);
      DEFINE(T_0b110);
      DEFINE(T_0b111);

      #undef DEFINE

      default:
        return typeid(float64);
      }
    }

    auto write(std::ostream & os) const -> std::ostream &
    {
      switch (signature())
      {
      #define DEFINE(TYPE)                                                     \
      case signature_##TYPE:                                                   \
        if constexpr (std::is_same_v<TYPE, bool>)                              \
        {                                                                      \
          return os << std::boolalpha << yellow('#', as<TYPE>());              \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          return os << yellow(as<TYPE>());                                     \
        }                                                                      \
        static_assert(true)

      DEFINE(pointer);
      DEFINE(T_0b010);
      DEFINE(T_0b011);
      DEFINE(T_0b100);
      DEFINE(T_0b101);
      DEFINE(T_0b110);
      DEFINE(T_0b111);

      #undef DEFINE

      default:
        return os << as<float64>();
      }
    }
  };

  template <typename... Ts>
  constexpr auto operator ==(nan_boxing_pointer<Ts...> const& x,
                             nan_boxing_pointer<Ts...> const& y)
  {
    return x.equivalent_to(y);
  }

  template <typename... Ts>
  constexpr auto operator !=(nan_boxing_pointer<Ts...> const& x,
                             nan_boxing_pointer<Ts...> const& y)
  {
    return not x.equivalent_to(y);
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

