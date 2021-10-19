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

#ifndef INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
#define INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP

#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/ratio.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct floating_point : public std::numeric_limits<T>
  {
    using value_type = T;

    value_type value;

    explicit constexpr floating_point(T value = {})
      : value { value }
    {}

    explicit floating_point(std::string const& token) try
      : value { lexical_cast<value_type>(token) }
    {}
    catch (...)
    {
      throw read_error(make<string>("not a decimal"), make<string>(token));
    }

    auto exact() const -> object
    {
      /* ---- R7RS 6.2.6 (exact z) ---------------------------------------------
       *
       *  The procedure exact returns an exact representation of z. The value
       *  returned is the exact number that is numerically closest to the
       *  argument. For exact arguments, the result is the same as the argument.
       *  For inexact non-integral real arguments, the implementation may return
       *  a rational approximation, or may report an implementation
       *
       * -------------------------------------------------------------------- */

      return ratio(value).simple();
    }

    auto is_integer() const noexcept
    {
      return value == std::trunc(value);
    }

    // TODO TEMPLATE SPECIALIZATION to<std::string>()
    auto to_string() const
    {
      return lexical_cast<std::string>(value);
    }

    auto inexact() const noexcept
    {
      return make(floating_point<double>(value));
    }

    #define DEFINE(NAME)                                                       \
    auto NAME() const                                                          \
    {                                                                          \
      return make(floating_point(std::NAME(value)));                           \
    }                                                                          \
    static_assert(true)

    DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh); DEFINE(exp);
    DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh); DEFINE(log);
    DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh); DEFINE(sqrt);

    DEFINE(floor);
    DEFINE(ceil);
    DEFINE(trunc);
    DEFINE(round);

    #undef DEFINE

    #define DEFINE(NAME)                                                       \
    auto NAME(const_reference x) const                                         \
    {                                                                          \
      return make(floating_point(std::NAME(value, x.inexact().as<double_float>()))); \
    }                                                                          \
    static_assert(true)

    DEFINE(atan2);
    DEFINE(pow);

    #undef DEFINE

    constexpr operator value_type() const noexcept { return value; }
    constexpr operator value_type()       noexcept { return value; }
  };

  template <typename T>
  auto operator <<(std::ostream & os, floating_point<T> const& rhs) -> std::ostream &
  {
    if (std::isnan(rhs))
    {
      return os << cyan << "+nan.0" << reset;
    }
    else if (std::isinf(rhs))
    {
      return os << cyan << (0 < rhs.value ? '+' : '-') << "inf.0" << reset;
    }
    else
    {
      return os << cyan << std::fixed << rhs.value << reset;
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
