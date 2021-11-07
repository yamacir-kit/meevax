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

#ifndef INCLUDED_MEEVAX_KERNEL_RATIO_HPP
#define INCLUDED_MEEVAX_KERNEL_RATIO_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct ratio : public number
               , public virtual pair
  {
    using pair::pair;

    explicit ratio(double);

    explicit ratio(std::string const&, int = 0);

    auto exact() const -> object override;

    auto inexact() const -> object override;

    auto denominator() const -> exact_integer const&;

    auto invert() const -> ratio;

    auto is_complex() const noexcept -> bool override { return true; }

    auto is_real() const noexcept -> bool override { return true; }

    auto is_rational() const noexcept -> bool override { return true; }

    auto is_integer() const -> bool override;

    auto numerator() const -> exact_integer const&;

    auto reduce() const -> ratio;

    auto simple() const -> object;

    #define DEFINE(NAME) auto NAME() const -> object override

    DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh); DEFINE(exp);
    DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh); DEFINE(log);
    DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh); DEFINE(sqrt);

    DEFINE(floor);
    DEFINE(ceil);
    DEFINE(trunc);
    DEFINE(round);

    #undef DEFINE

    #define DEFINE(NAME) auto NAME(const_reference) const -> object override

    DEFINE(atan2);
    DEFINE(pow);

    #undef DEFINE

    auto operator + (const_reference) const -> object override;
    auto operator - (const_reference) const -> object override;
    auto operator * (const_reference) const -> object override;
    auto operator / (const_reference) const -> object override;
    auto operator % (const_reference) const -> object override;

    auto operator ==(const_reference) const -> bool override;
    auto operator !=(const_reference) const -> bool override;
    auto operator < (const_reference) const -> bool override;
    auto operator <=(const_reference) const -> bool override;
    auto operator > (const_reference) const -> bool override;
    auto operator >=(const_reference) const -> bool override;
  };

  auto operator <<(std::ostream &, ratio const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
