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

#ifndef INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP
#define INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP

#include <meevax/kernel/mnemonic.hpp>
#include <meevax/memory/gc_pointer.hpp>
#include <meevax/type_traits/requires.hpp>
#include <meevax/utility/demangle.hpp>

#define NIL /* nothing */

namespace meevax
{
inline namespace kernel
{
  class environment;

  struct error;          // error.hpp
  struct exact_integer;  // exact_integer.hpp
  struct pair;           // pair.hpp
  struct ratio;          // ratio.hpp

  template <typename T>
  struct floating_point; // floating_point.hpp

  using single_float = floating_point<float>;
  using double_float = floating_point<double>; // NOTE: 0.0 is double

  template <template <typename...> typename, typename, typename...>
  class heterogeneous;

  using value_type = heterogeneous<gc_pointer, pair, bool, std::int32_t, std::uint32_t, float, mnemonic>;

  using reference = value_type &;

  using const_reference = value_type const&;

  using let = value_type;

  using null = std::nullptr_t;

  struct number
  {
    #define DEFINE(NAME) virtual auto NAME() const -> value_type = 0

    DEFINE(exact); DEFINE(inexact);

    DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh); DEFINE(exp);
    DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh); DEFINE(log);
    DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh); DEFINE(sqrt);

    DEFINE(floor); DEFINE(ceil); DEFINE(trunc); DEFINE(round);

    #undef DEFINE

    virtual auto atan2(const_reference) const -> value_type = 0;
    virtual auto pow  (const_reference) const -> value_type = 0;

    virtual auto is_complex () const -> bool { return true ; }
    virtual auto is_real    () const -> bool { return false; }
    virtual auto is_rational() const -> bool { return false; }
    virtual auto is_integer () const -> bool { return false; }

    virtual auto is_finite  () const -> bool { return true ; }
    virtual auto is_infinite() const -> bool { return false; }
    virtual auto is_nan     () const -> bool { return false; }

    virtual auto operator + (const_reference) const -> value_type = 0;
    virtual auto operator - (const_reference) const -> value_type = 0;
    virtual auto operator * (const_reference) const -> value_type = 0;
    virtual auto operator / (const_reference) const -> value_type = 0;
    virtual auto operator % (const_reference) const -> value_type = 0;

    virtual auto operator ==(const_reference) const -> bool = 0;
    virtual auto operator !=(const_reference) const -> bool = 0;
    virtual auto operator < (const_reference) const -> bool = 0;
    virtual auto operator <=(const_reference) const -> bool = 0;
    virtual auto operator > (const_reference) const -> bool = 0;
    virtual auto operator >=(const_reference) const -> bool = 0;
  };

  #define DEFINE(SYMBOL)                                                       \
  template <template <typename...> typename P, typename T, typename... Ts>     \
  auto operator SYMBOL(heterogeneous<P, T, Ts...> const& x,                    \
                       heterogeneous<P, T, Ts...> const& y) -> decltype(auto)  \
  {                                                                            \
    return x.template as<number>() SYMBOL y;                                   \
  }                                                                            \
  static_assert(true)

  DEFINE(+);
  DEFINE(-);
  DEFINE(*);
  DEFINE(/);
  DEFINE(%);

  #undef DEFINE

  [[noreturn]]
  auto raise(std::string const&) -> void; // error.hpp
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP
