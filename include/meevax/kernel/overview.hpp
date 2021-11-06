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

#ifndef INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP
#define INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP

#include <meevax/memory/gc_pointer.hpp>
#include <meevax/string/append.hpp>
#include <meevax/type_traits/requires.hpp>

#define NIL /* nothing */

#define PROFILE_ALLOCATION false

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

  template <template <typename...> typename Pointer, typename T>
  class heterogeneous;

  using                         object = heterogeneous<gc_pointer, pair>;
  using                   let = object;
  using       reference = let      &;
  using const_reference = let const&;

  using null = std::nullptr_t;

  struct number
  {
    #define DEFINE(NAME) virtual auto NAME() const -> object = 0

    DEFINE(exact); DEFINE(inexact);

    DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh); DEFINE(exp);
    DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh); DEFINE(log);
    DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh); DEFINE(sqrt);

    DEFINE(floor); DEFINE(ceil); DEFINE(trunc); DEFINE(round);

    #undef DEFINE

    virtual auto atan2(const_reference) const -> object = 0;
    virtual auto pow  (const_reference) const -> object = 0;

    virtual auto is_complex () const -> bool { return true ; }
    virtual auto is_real    () const -> bool { return false; }
    virtual auto is_rational() const -> bool { return false; }
    virtual auto is_integer () const -> bool { return false; }

    virtual auto is_finite  () const -> bool { return true ; }
    virtual auto is_infinite() const -> bool { return false; }
    virtual auto is_nan     () const -> bool { return false; }

    virtual auto operator + (const_reference) const -> object = 0;
    virtual auto operator - (const_reference) const -> object = 0;
    virtual auto operator * (const_reference) const -> object = 0;
    virtual auto operator / (const_reference) const -> object = 0;
    virtual auto operator % (const_reference) const -> object = 0;

    virtual auto operator ==(const_reference) const -> bool = 0;
    virtual auto operator !=(const_reference) const -> bool = 0;
    virtual auto operator < (const_reference) const -> bool = 0;
    virtual auto operator <=(const_reference) const -> bool = 0;
    virtual auto operator > (const_reference) const -> bool = 0;
    virtual auto operator >=(const_reference) const -> bool = 0;
  };

  [[noreturn]]
  auto raise(std::string const&) -> void; // error.hpp
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP
