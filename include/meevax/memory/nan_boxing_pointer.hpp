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

#ifndef INCLUDED_MEEVAX_MEMORY_NAN_BOXING_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_NAN_BOXING_POINTER_HPP

#include <bit>
#include <charconv>
#include <cmath>
#include <cstddef>
#include <iomanip>
#include <limits>
#include <ostream>

#include <meevax/concepts/any_of.hpp>
#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/kernel/small_integer.hpp>

namespace meevax::inline memory
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

    template <typename U>
    using payload_t = std::conditional_t<sizeof(U) <= 1, std::uint8_t, std::conditional_t<sizeof(U) <= 2, std::uint16_t, std::conditional_t<sizeof(U) <= 4, std::uint32_t, std::uint64_t>>>;

    using pointer = T *;

    std::uintptr_t data;

    auto static constexpr mask_sign      = std::uintptr_t(0b1000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000);
    auto static constexpr mask_exponent  = std::uintptr_t(0b0111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000);
    auto static constexpr mask_fraction  = std::uintptr_t(0b0000'0000'0000'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111);
    auto static constexpr mask_quiet     = std::uintptr_t(0b0000'0000'0000'1000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000);
    auto static constexpr mask_type      = std::uintptr_t(0b0000'0000'0000'0111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000);
    auto static constexpr mask_signature = std::uintptr_t(0b0111'1111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000);
    auto static constexpr mask_payload   = std::uintptr_t(0b0000'0000'0000'0000'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111'1111);

    template <typename Tn>
    auto static constexpr signature_of() noexcept -> std::uintptr_t
    {
           if constexpr (std::is_same_v<Tn, double>) { return 0b0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000; }
      else if constexpr (std::is_same_v<Tn, T1    >) { return 0b0111'1111'1111'1001'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000; }
      else if constexpr (std::is_same_v<Tn, T2    >) { return 0b0111'1111'1111'1010'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000; }
      else if constexpr (std::is_same_v<Tn, T3    >) { return 0b0111'1111'1111'1011'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000; }
      else if constexpr (std::is_same_v<Tn, T4    >) { return 0b0111'1111'1111'1100'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000; }
      else if constexpr (std::is_same_v<Tn, T5    >) { return 0b0111'1111'1111'1101'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000; }
      else if constexpr (std::is_same_v<Tn, T6    >) { return 0b0111'1111'1111'1110'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000; }
      else if constexpr (std::is_same_v<Tn, T*    >) { return 0b0111'1111'1111'1111'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000'0000; }
      else
      {
        static_assert([]() { return false; });
      }
    }

    constexpr nan_boxing_pointer(nan_boxing_pointer const&) = default;

    explicit constexpr nan_boxing_pointer(double x) noexcept
      : data { std::bit_cast<std::uintptr_t>(canonicalize(x)) }
    {}

    template <any_of<T1, T2, T3, T4, T5, T6> Tn>
    explicit constexpr nan_boxing_pointer(Tn x) noexcept
      : data { signature_of<Tn>() | std::bit_cast<payload_t<Tn>>(x) }
    {}

    explicit constexpr nan_boxing_pointer(pointer x = nullptr) noexcept
      : data { signature_of<T*>() | std::bit_cast<payload_t<T*>>(x) }
    {}

    auto operator =(auto x) noexcept -> auto &
    {
      reset(std::forward<decltype(x)>(x));
      return *this;
    }

    auto constexpr operator ->() const noexcept
    {
      return unsafe_get();
    }

    auto constexpr operator *() const noexcept -> auto const&
    {
      return *unsafe_get();
    }

    auto constexpr operator *() noexcept -> auto &
    {
      return *unsafe_get();
    }

    explicit constexpr operator bool() const noexcept
    {
      return dereferenceable() ? unsafe_get() != nullptr : false;
    }

    template <typename U>
    auto constexpr as() const noexcept
    {
      if constexpr (std::is_same_v<std::decay_t<U>, double>)
      {
        return std::bit_cast<double>(data);
      }
      else
      {
        return std::bit_cast<std::decay_t<U>>(static_cast<payload_t<std::decay_t<U>>>(payload()));
      }
    }

    auto static constexpr canonicalize(double x) noexcept -> double
    {
      return std::isnan(x) ? std::bit_cast<double>(mask_exponent | mask_quiet) : x;
    }

    auto constexpr dereferenceable() const noexcept
    {
      return signature() == signature_of<pointer>();
    }

    auto constexpr get() const noexcept -> pointer
    {
      return dereferenceable() ? unsafe_get() : nullptr;
    }

    template <typename U>
    auto constexpr is() const noexcept
    {
      switch (signature())
      {
      case signature_of<T1>(): return std::is_same_v<std::decay_t<U>, T1>;
      case signature_of<T2>(): return std::is_same_v<std::decay_t<U>, T2>;
      case signature_of<T3>(): return std::is_same_v<std::decay_t<U>, T3>;
      case signature_of<T4>(): return std::is_same_v<std::decay_t<U>, T4>;
      case signature_of<T5>(): return std::is_same_v<std::decay_t<U>, T5>;
      case signature_of<T6>(): return std::is_same_v<std::decay_t<U>, T6>;
      case signature_of<T*>(): return std::is_same_v<std::decay_t<U>, T*>;

      default:
        return std::is_same_v<std::decay_t<U>, double>;
      }
    }

    auto reset(nan_boxing_pointer const& other) noexcept -> void
    {
      data = other.data;
    }

    auto reset(double x) noexcept -> void
    {
      data = std::bit_cast<std::uintptr_t>(canonicalize(x));
    }

    template <any_of<T1, T2, T3, T4, T5, T6> Tn>
    auto reset(Tn x) noexcept -> void
    {
      data = signature_of<Tn>() | std::bit_cast<payload_t<Tn>>(x);
    }

    auto reset(pointer x = nullptr) noexcept -> void
    {
      data = signature_of<T*>() | std::bit_cast<payload_t<T*>>(x);
    }

    auto constexpr payload() const noexcept
    {
      return data & mask_payload;
    }

    auto constexpr signature() const noexcept
    {
      return data & mask_signature;
    }

    auto constexpr type() const noexcept -> std::type_info const&
    {
      switch (signature())
      {
      case signature_of<T1>(): return typeid(T1);
      case signature_of<T2>(): return typeid(T2);
      case signature_of<T3>(): return typeid(T3);
      case signature_of<T4>(): return typeid(T4);
      case signature_of<T5>(): return typeid(T5);
      case signature_of<T6>(): return typeid(T6);
      case signature_of<T*>(): return typeid(T*);

      default:
        return typeid(double);
      }
    }

    auto constexpr unsafe_get() const noexcept
    {
      return reinterpret_cast<pointer>(payload());
    }

    template <any_of<T1, T2, T3, T4, T5, T6, T*> Tn>
    auto static write(std::ostream & os, Tn x) -> std::ostream &
    {
      if constexpr (std::is_same_v<decltype(x), bool>)
      {
        return os << cyan('#', x ? 't' : 'f');
      }
      else if constexpr (std::is_arithmetic_v<decltype(x)>)
      {
        return os << std::dec << cyan(x);
      }
      else
      {
        return os << cyan(x);
      }
    }

    auto write(std::ostream & os) const -> std::ostream &
    {
      switch (signature())
      {
      case signature_of<T1>(): return write(os, as<T1>());
      case signature_of<T2>(): return write(os, as<T2>());
      case signature_of<T3>(): return write(os, as<T3>());
      case signature_of<T4>(): return write(os, as<T4>());
      case signature_of<T5>(): return write(os, as<T5>());
      case signature_of<T6>(): return write(os, as<T6>());
      case signature_of<T*>(): return write(os, as<T*>());

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
          char buffer[48] = {};
          std::to_chars(buffer, buffer + sizeof(buffer), value, std::chars_format::general);
          return os << cyan(buffer);
        }
      }
    }
  };

  template <typename... Ts> auto constexpr operator ==(nan_boxing_pointer<Ts...> const& x, nan_boxing_pointer<Ts...> const& y) { return x.data == y.data; }
  template <typename... Ts> auto constexpr operator !=(nan_boxing_pointer<Ts...> const& x, nan_boxing_pointer<Ts...> const& y) { return x.data != y.data; }
} // namespace meevax::memory

template <typename... Ts>
struct std::hash<meevax::memory::nan_boxing_pointer<Ts...>>
  : public std::hash<typename meevax::memory::nan_boxing_pointer<Ts...>::pointer>
{};

#endif // INCLUDED_MEEVAX_MEMORY_NAN_BOXING_POINTER_HPP
