/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#include <bit>
#include <cstddef>
#include <cmath>
#include <iomanip>
#include <ostream>
#include <typeinfo>

#include <meevax/type_traits/integer.hpp>

namespace meevax
{
inline namespace memory
{
  static_assert(std::numeric_limits<double>::is_iec559 and sizeof(double) == 8);

  template <typename T,
            typename T1 = std::integral_constant<int, 1>,
            typename T2 = std::integral_constant<int, 2>,
            typename T3 = std::integral_constant<int, 3>,
            typename T4 = std::integral_constant<int, 4>,
            typename T5 = std::integral_constant<int, 5>,
            typename T6 = std::integral_constant<int, 6>>
  struct nan_boxing_pointer
  {
    static_assert(sizeof(T1) <= 6 or std::is_pointer_v<T1>);
    static_assert(sizeof(T2) <= 6 or std::is_pointer_v<T2>);
    static_assert(sizeof(T3) <= 6 or std::is_pointer_v<T3>);
    static_assert(sizeof(T4) <= 6 or std::is_pointer_v<T4>);
    static_assert(sizeof(T5) <= 6 or std::is_pointer_v<T5>);
    static_assert(sizeof(T6) <= 6 or std::is_pointer_v<T6>);

    using element_type = std::decay_t<T>;

    using pointer = std::add_pointer_t<element_type>;

    pointer data;

    static constexpr std::uintptr_t mask_sign         = 0b1000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t mask_exponent     = 0b0111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t mask_fraction     = 0b0000'0000'0000'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111;
    static constexpr std::uintptr_t mask_quiet        = 0b0000'0000'0000'1000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t mask_type         = 0b0000'0000'0000'0111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;

    static constexpr std::uintptr_t mask_signature    = 0b0111'1111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t mask_payload      = 0b0000'0000'0000'0000'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111;

    static constexpr std::uintptr_t signature_double  = 0b0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T1      = 0b0111'1111'1111'1001'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T2      = 0b0111'1111'1111'1010'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T3      = 0b0111'1111'1111'1011'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T4      = 0b0111'1111'1111'1100'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T5      = 0b0111'1111'1111'1101'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_T6      = 0b0111'1111'1111'1110'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;
    static constexpr std::uintptr_t signature_pointer = 0b0111'1111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000;

    constexpr nan_boxing_pointer(nan_boxing_pointer const&) = default;

    #define DEFINE(TYPE, ...)                                                  \
    explicit nan_boxing_pointer(TYPE const& value __VA_ARGS__) noexcept        \
      : data { reinterpret_cast<pointer>(signature_##TYPE | std::bit_cast<uint8n_t<sizeof(TYPE)>>(value)) } \
    {}                                                                         \
                                                                               \
    auto reset(TYPE const& value __VA_ARGS__) noexcept -> void                 \
    {                                                                          \
      data = reinterpret_cast<pointer>(signature_##TYPE | std::bit_cast<uint8n_t<sizeof(TYPE)>>(value)); \
    }

    DEFINE(double,           )
    DEFINE(T1,               )
    DEFINE(T2,               )
    DEFINE(T3,               )
    DEFINE(T4,               )
    DEFINE(T5,               )
    DEFINE(T6,               )
    DEFINE(pointer, = nullptr)

    #undef DEFINE

    template <typename... Ts>
    auto operator =(Ts&&... xs) -> auto &
    {
      reset(std::forward<decltype(xs)>(xs)...);
      return *this;
    }

    auto operator ->() const
    {
      return unsafe_get();
    }

    auto operator *() const -> auto const&
    {
      return *unsafe_get();
    }

    auto operator *() -> auto &
    {
      return *unsafe_get();
    }

    explicit operator bool() const noexcept
    {
      return dereferenceable() ? unsafe_get() != nullptr : false;
    }

    template <typename U>
    auto as() const
    {
      if constexpr (std::is_same_v<std::decay_t<U>, double>)
      {
        return std::bit_cast<double>(data);
      }
      else
      {
        return std::bit_cast<std::decay_t<U>>(static_cast<uint8n_t<sizeof(std::decay_t<U>)>>(payload()));
      }
    }

    auto dereferenceable() const noexcept
    {
      return signature() == signature_pointer;
    }

    auto compare(nan_boxing_pointer const& nbp) const noexcept
    {
      return data == nbp.data;
    }

    auto get() const noexcept -> pointer
    {
      return dereferenceable() ? unsafe_get() : nullptr;
    }

    template <typename U>
    auto is() const noexcept
    {
      return type() == typeid(std::decay_t<U>);
    }

    auto reset(nan_boxing_pointer const& value) -> void
    {
      data = value.data;
    }

    auto payload() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(data) & mask_payload;
    }

    auto signature() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(data) & mask_signature;
    }

    auto type() const noexcept -> decltype(auto)
    {
      switch (signature())
      {
      #define DEFINE(TYPE) case signature_##TYPE: return typeid(TYPE)

      DEFINE(T1);
      DEFINE(T2);
      DEFINE(T3);
      DEFINE(T4);
      DEFINE(T5);
      DEFINE(T6);
      DEFINE(pointer);

      #undef DEFINE

      default:
        return typeid(double);
      }
    }

    auto unsafe_get() const noexcept
    {
      return reinterpret_cast<pointer>(payload());
    }

    auto write(std::ostream & os) const -> std::ostream &
    {
      switch (signature())
      {
      #define CASE(TYPE)                                                       \
      case signature_##TYPE:                                                   \
        if constexpr (std::is_same_v<TYPE, bool>)                              \
        {                                                                      \
          return os << yellow('#', as<TYPE>() ? 't' : 'f');                    \
        }                                                                      \
        else                                                                   \
        {                                                                      \
          return os << yellow(as<TYPE>());                                     \
        }                                                                      \
        static_assert(true)

      CASE(T1);
      CASE(T2);
      CASE(T3);
      CASE(T4);
      CASE(T5);
      CASE(T6);
      CASE(pointer);

      #undef CASE

      default:
        if (auto value = as<double>(); std::isnan(value))
        {
          return os << cyan("+nan.0");
        }
        else if (std::isinf(value))
        {
          return os << cyan(0 < value ? '+' : '-', "inf.0");
        }
        else
        {
          return os << std::fixed << std::setprecision(17) << cyan(value);
        }
      }
    }
  };

  template <typename... Ts>
  constexpr auto operator ==(nan_boxing_pointer<Ts...> const& x,
                             nan_boxing_pointer<Ts...> const& y)
  {
    return x.compare(y);
  }

  template <typename... Ts>
  constexpr auto operator !=(nan_boxing_pointer<Ts...> const& x,
                             nan_boxing_pointer<Ts...> const& y)
  {
    return not x.compare(y);
  }
} // namespace memory
} // namespace meevax

template <typename... Ts>
struct std::hash<meevax::memory::nan_boxing_pointer<Ts...>>
  : public std::hash<typename meevax::memory::nan_boxing_pointer<Ts...>::pointer>
{};

#endif // INCLUDED_MEEVAX_MEMORY_NAN_BOXING_POINTER_HPP

