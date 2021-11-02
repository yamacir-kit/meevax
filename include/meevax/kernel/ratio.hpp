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

#include <valarray>

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct ratio : public number
               , public virtual pair
  {
    using pair::pair;

    template <typename T, REQUIRES(std::is_floating_point<T>)>
    explicit ratio(T x, T const e = std::numeric_limits<double>::epsilon())
    {
      int sign  = x > 0 ? 1 : -1;

      x = std::abs(x);

      std::valarray<T> v1 { static_cast<T>(static_cast<int>(x)), 1 },
                       v2 { 1, 0 };

      /* ---- Continued Fraction Expantion -------------------------------------
       *
       *                        1
       *  x_0 = a_0 + ---------------------
       *                           1
       *              a_1 + ---------------
       *                              1
       *                    a_2 + ---------
       *                                 1
       *                          a_n + ---
       *                                 e
       *
       * -------------------------------------------------------------------- */
      auto x_n = x - static_cast<int>(x);

      while (e < x_n)
      {
        auto a_n = 1 / x_n;

        x_n = a_n - static_cast<int>(a_n);

        auto old_1 = v1;
        v1 = static_cast<T>(static_cast<int>(a_n)) * v1 + v2;
        v2 = old_1;
      }

      std::get<0>(*this) = make<exact_integer>(sign * v1[0]);
      std::get<1>(*this) = make<exact_integer>(       v1[1]);
    }

    explicit ratio(std::string const&, int = 0);

    auto exact() const -> object override;

    auto inexact() const -> object override;

    auto denominator() const -> exact_integer const&;

    auto invert() const -> ratio;

    auto is_integer() const -> bool override;

    auto is_nan() const -> bool override { return false; }

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
  };

  auto operator <<(std::ostream &, ratio const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
