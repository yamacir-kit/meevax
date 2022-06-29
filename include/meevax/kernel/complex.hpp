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

#ifndef INCLUDED_MEEVAX_KERNEL_COMPLEX_HPP
#define INCLUDED_MEEVAX_KERNEL_COMPLEX_HPP

#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct complex : public number
                 , public virtual pair
  {
    using pair::pair;

    auto real() const noexcept -> const_reference;

    auto real() noexcept -> reference;

    auto imag() const noexcept -> const_reference;

    auto imag() noexcept -> reference;

    #define DEFINE(NAME)                                                       \
    auto NAME() const -> value_type override                                   \
    {                                                                          \
      return unspecified_object;                                               \
    }                                                                          \
    static_assert(true)

    DEFINE(exact); DEFINE(inexact);

    DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh); DEFINE(exp);
    DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh); DEFINE(log);
    DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh); DEFINE(sqrt);

    DEFINE(floor); DEFINE(ceil); DEFINE(trunc); DEFINE(round);

    #undef DEFINE

    #define DEFINE(NAME)                                                       \
    auto NAME(const_reference) const -> value_type override                    \
    {                                                                          \
      return unspecified_object;                                               \
    }                                                                          \
    static_assert(true)

    DEFINE(atan2);
    DEFINE(pow);

    #undef DEFINE

    auto operator + (const_reference) const -> value_type override { return unspecified_object; }
    auto operator - (const_reference) const -> value_type override { return unspecified_object; }
    auto operator * (const_reference) const -> value_type override { return unspecified_object; }
    auto operator / (const_reference) const -> value_type override { return unspecified_object; }
    auto operator % (const_reference) const -> value_type override { return unspecified_object; }

    auto operator ==(const_reference) const -> bool override { return false; };
    auto operator !=(const_reference) const -> bool override { return false; };
    auto operator < (const_reference) const -> bool override { return false; };
    auto operator <=(const_reference) const -> bool override { return false; };
    auto operator > (const_reference) const -> bool override { return false; };
    auto operator >=(const_reference) const -> bool override { return false; };
  };

  auto operator <<(std::ostream &, complex const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_COMPLEX_HPP
