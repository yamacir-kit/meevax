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

#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/ratio.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator * (exact_integer const&, exact_integer const&) -> exact_integer;
  auto operator + (exact_integer const&, exact_integer const&) -> exact_integer;
  auto operator - (exact_integer const&, exact_integer const&) -> exact_integer;
  auto operator / (exact_integer const&, exact_integer const&) -> ratio;
  auto operator % (exact_integer const&, exact_integer const&) -> exact_integer;
  auto operator !=(exact_integer const&, exact_integer const&) -> bool;
  auto operator < (exact_integer const&, exact_integer const&) -> bool;
  auto operator <=(exact_integer const&, exact_integer const&) -> bool;
  auto operator ==(exact_integer const&, exact_integer const&) -> bool;
  auto operator > (exact_integer const&, exact_integer const&) -> bool;
  auto operator >=(exact_integer const&, exact_integer const&) -> bool;

  auto operator * (exact_integer const&, ratio const&) -> ratio;
  auto operator + (exact_integer const&, ratio const&) -> ratio;
  auto operator - (exact_integer const&, ratio const&) -> ratio;
  auto operator / (exact_integer const&, ratio const&) -> ratio;
  auto operator % (exact_integer const&, ratio const&) -> ratio;
  auto operator !=(exact_integer const&, ratio const&) -> bool;
  auto operator < (exact_integer const&, ratio const&) -> bool;
  auto operator <=(exact_integer const&, ratio const&) -> bool;
  auto operator ==(exact_integer const&, ratio const&) -> bool;
  auto operator > (exact_integer const&, ratio const&) -> bool;
  auto operator >=(exact_integer const&, ratio const&) -> bool;

  auto operator * (exact_integer const&, float) -> float;
  auto operator + (exact_integer const&, float) -> float;
  auto operator - (exact_integer const&, float) -> float;
  auto operator / (exact_integer const&, float) -> float;
  auto operator % (exact_integer const&, float) -> float;
  auto operator !=(exact_integer const&, float) -> bool;
  auto operator < (exact_integer const&, float) -> bool;
  auto operator <=(exact_integer const&, float) -> bool;
  auto operator ==(exact_integer const&, float) -> bool;
  auto operator > (exact_integer const&, float) -> bool;
  auto operator >=(exact_integer const&, float) -> bool;

  auto operator * (exact_integer const&, double) -> double;
  auto operator + (exact_integer const&, double) -> double;
  auto operator - (exact_integer const&, double) -> double;
  auto operator / (exact_integer const&, double) -> double;
  auto operator % (exact_integer const&, double) -> double;
  auto operator !=(exact_integer const&, double) -> bool;
  auto operator < (exact_integer const&, double) -> bool;
  auto operator <=(exact_integer const&, double) -> bool;
  auto operator ==(exact_integer const&, double) -> bool;
  auto operator > (exact_integer const&, double) -> bool;
  auto operator >=(exact_integer const&, double) -> bool;

  auto operator * (exact_integer const&, complex const&) -> complex;
  auto operator + (exact_integer const&, complex const&) -> complex;
  auto operator - (exact_integer const&, complex const&) -> complex;
  auto operator / (exact_integer const&, complex const&) -> complex;
  auto operator ==(exact_integer const&, complex const&) -> bool;
  auto operator !=(exact_integer const&, complex const&) -> bool;

  auto operator * (ratio const&, exact_integer const&) -> ratio;
  auto operator + (ratio const&, exact_integer const&) -> ratio;
  auto operator - (ratio const&, exact_integer const&) -> ratio;
  auto operator / (ratio const&, exact_integer const&) -> ratio;
  auto operator % (ratio const&, exact_integer const&) -> ratio;
  auto operator !=(ratio const&, exact_integer const&) -> bool;
  auto operator < (ratio const&, exact_integer const&) -> bool;
  auto operator <=(ratio const&, exact_integer const&) -> bool;
  auto operator ==(ratio const&, exact_integer const&) -> bool;
  auto operator > (ratio const&, exact_integer const&) -> bool;
  auto operator >=(ratio const&, exact_integer const&) -> bool;

  auto operator * (ratio const&, ratio const&) -> ratio;
  auto operator + (ratio const&, ratio const&) -> ratio;
  auto operator - (ratio const&, ratio const&) -> ratio;
  auto operator / (ratio const&, ratio const&) -> ratio;
  auto operator % (ratio const&, ratio const&) -> ratio;
  auto operator ==(ratio const&, ratio const&) -> bool;
  auto operator !=(ratio const&, ratio const&) -> bool;
  auto operator < (ratio const&, ratio const&) -> bool;
  auto operator <=(ratio const&, ratio const&) -> bool;
  auto operator > (ratio const&, ratio const&) -> bool;
  auto operator >=(ratio const&, ratio const&) -> bool;

  auto operator + (ratio const&, float) -> float;
  auto operator - (ratio const&, float) -> float;
  auto operator * (ratio const&, float) -> float;
  auto operator / (ratio const&, float) -> float;
  auto operator % (ratio const&, float) -> float;
  auto operator ==(ratio const&, float) -> bool;
  auto operator !=(ratio const&, float) -> bool;
  auto operator < (ratio const&, float) -> bool;
  auto operator <=(ratio const&, float) -> bool;
  auto operator > (ratio const&, float) -> bool;
  auto operator >=(ratio const&, float) -> bool;

  auto operator + (ratio const&, double) -> double;
  auto operator - (ratio const&, double) -> double;
  auto operator * (ratio const&, double) -> double;
  auto operator / (ratio const&, double) -> double;
  auto operator % (ratio const&, double) -> double;
  auto operator ==(ratio const&, double) -> bool;
  auto operator !=(ratio const&, double) -> bool;
  auto operator < (ratio const&, double) -> bool;
  auto operator <=(ratio const&, double) -> bool;
  auto operator > (ratio const&, double) -> bool;
  auto operator >=(ratio const&, double) -> bool;

  auto operator + (ratio const&, complex const&) -> complex;
  auto operator - (ratio const&, complex const&) -> complex;
  auto operator * (ratio const&, complex const&) -> complex;
  auto operator / (ratio const&, complex const&) -> complex;
  auto operator ==(ratio const&, complex const&) -> bool;
  auto operator !=(ratio const&, complex const&) -> bool;

  auto operator + (float, exact_integer const&) -> float;
  auto operator - (float, exact_integer const&) -> float;
  auto operator * (float, exact_integer const&) -> float;
  auto operator / (float, exact_integer const&) -> float;
  auto operator % (float, exact_integer const&) -> float;
  auto operator ==(float, exact_integer const&) -> bool;
  auto operator !=(float, exact_integer const&) -> bool;
  auto operator < (float, exact_integer const&) -> bool;
  auto operator <=(float, exact_integer const&) -> bool;
  auto operator > (float, exact_integer const&) -> bool;
  auto operator >=(float, exact_integer const&) -> bool;

  auto operator + (float, ratio const&) -> float;
  auto operator - (float, ratio const&) -> float;
  auto operator * (float, ratio const&) -> float;
  auto operator / (float, ratio const&) -> float;
  auto operator % (float, ratio const&) -> float;
  auto operator ==(float, ratio const&) -> bool;
  auto operator !=(float, ratio const&) -> bool;
  auto operator < (float, ratio const&) -> bool;
  auto operator <=(float, ratio const&) -> bool;
  auto operator > (float, ratio const&) -> bool;
  auto operator >=(float, ratio const&) -> bool;

  auto operator + (float, complex const&) -> complex;
  auto operator - (float, complex const&) -> complex;
  auto operator * (float, complex const&) -> complex;
  auto operator / (float, complex const&) -> complex;
  auto operator ==(float, complex const&) -> bool;
  auto operator !=(float, complex const&) -> bool;

  auto operator + (double, exact_integer const&) -> double;
  auto operator - (double, exact_integer const&) -> double;
  auto operator * (double, exact_integer const&) -> double;
  auto operator / (double, exact_integer const&) -> double;
  auto operator % (double, exact_integer const&) -> double;
  auto operator ==(double, exact_integer const&) -> bool;
  auto operator !=(double, exact_integer const&) -> bool;
  auto operator < (double, exact_integer const&) -> bool;
  auto operator <=(double, exact_integer const&) -> bool;
  auto operator > (double, exact_integer const&) -> bool;
  auto operator >=(double, exact_integer const&) -> bool;

  auto operator + (double, ratio const&) -> double;
  auto operator - (double, ratio const&) -> double;
  auto operator * (double, ratio const&) -> double;
  auto operator / (double, ratio const&) -> double;
  auto operator % (double, ratio const&) -> double;
  auto operator ==(double, ratio const&) -> bool;
  auto operator !=(double, ratio const&) -> bool;
  auto operator < (double, ratio const&) -> bool;
  auto operator <=(double, ratio const&) -> bool;
  auto operator > (double, ratio const&) -> bool;
  auto operator >=(double, ratio const&) -> bool;

  auto operator + (double, complex const&) -> complex;
  auto operator - (double, complex const&) -> complex;
  auto operator * (double, complex const&) -> complex;
  auto operator / (double, complex const&) -> complex;
  auto operator ==(double, complex const&) -> bool;
  auto operator !=(double, complex const&) -> bool;

  auto operator + (complex const&, complex const&) -> complex;
  auto operator - (complex const&, complex const&) -> complex;
  auto operator * (complex const&, complex const&) -> complex;
  auto operator / (complex const&, complex const&) -> complex;
  auto operator ==(complex const&, complex const&) -> bool;
  auto operator !=(complex const&, complex const&) -> bool;

  auto operator + (complex const&, float) -> complex;
  auto operator - (complex const&, float) -> complex;
  auto operator * (complex const&, float) -> complex;
  auto operator / (complex const&, float) -> complex;
  auto operator ==(complex const&, float) -> bool;
  auto operator !=(complex const&, float) -> bool;

  auto operator + (complex const&, double) -> complex;
  auto operator - (complex const&, double) -> complex;
  auto operator * (complex const&, double) -> complex;
  auto operator / (complex const&, double) -> complex;
  auto operator ==(complex const&, double) -> bool;
  auto operator !=(complex const&, double) -> bool;

  auto operator + (complex const&, ratio const&) -> complex;
  auto operator - (complex const&, ratio const&) -> complex;
  auto operator * (complex const&, ratio const&) -> complex;
  auto operator / (complex const&, ratio const&) -> complex;
  auto operator ==(complex const&, ratio const&) -> bool;
  auto operator !=(complex const&, ratio const&) -> bool;

  auto operator + (complex const&, exact_integer const&) -> complex;
  auto operator - (complex const&, exact_integer const&) -> complex;
  auto operator * (complex const&, exact_integer const&) -> complex;
  auto operator / (complex const&, exact_integer const&) -> complex;
  auto operator ==(complex const&, exact_integer const&) -> bool;
  auto operator !=(complex const&, exact_integer const&) -> bool;

  auto operator + (object const&, object const&) -> object;
  auto operator - (object const&, object const&) -> object;
  auto operator * (object const&, object const&) -> object;
  auto operator / (object const&, object const&) -> object;
  auto operator % (object const&, object const&) -> object;

  auto make_integer(std::string const&, int = 10) -> object;

  auto make_rational(std::string const&, int = 10) -> object;

  auto make_real(std::string const&, int = 10) -> object;

  auto make_complex(std::string const&, int = 10) -> object;

  auto make_number(std::string const&, int = 10) -> object;

  template <typename T, typename U>
  auto inexact_equals(T const& x, U const& y)
  {
    if constexpr (std::is_floating_point_v<T> and
                  std::is_floating_point_v<U>)
    {
      if (std::isnan(x) and std::isnan(y))
      {
        return true;
      }
      else if (std::isinf(x) or std::isinf(y))
      {
        return x == y;
      }
      else
      {
        using R = std::decay_t<decltype(std::declval<T>() - std::declval<U>())>;

        return std::abs(x - y) <= std::numeric_limits<R>::epsilon();
      }
    }
    else
    {
      return x == y;
    }
  }

inline namespace number
{
  auto equals(object const&, object const&) -> bool;

  auto not_equals(object const&, object const&) -> bool;

  auto less_than(object const&, object const&) -> bool;

  auto less_than_or_equals(object const&, object const&) -> bool;

  auto greater_than(object const&, object const&) -> bool;

  auto greater_than_or_equals(object const&, object const&) -> bool;

  auto exact(object const&) -> object;

  auto inexact(object const&) -> object;

  auto is_complex(object const&) -> bool;

  auto is_real(object const&) -> bool;

  auto is_rational(object const&) -> bool;

  auto is_integer(object const&) -> bool;

  auto is_exact(object const&) -> bool;

  auto is_inexact(object const&) -> bool;

  auto is_finite(object const&) -> bool;

  auto is_infinite(object const&) -> bool;

  auto is_nan(object const&) -> bool;

  auto is_zero(object const&) -> bool;

  auto is_positive(object const&) -> bool;

  auto is_negative(object const&) -> bool;

  auto is_odd(object const&) -> bool;

  auto is_even(object const&) -> bool;

  auto max(object const&) -> object;

  auto min(object const&) -> object;

  auto abs(object const&) -> object;

  auto quotient(object const&, object const&) -> object;

  auto remainder(object const&, object const&) -> object;

  auto modulo(object const&, object const&) -> object;

  auto gcd(object const&, object const&) -> object;

  auto lcm(object const&, object const&) -> object;

  auto sqrt(object const&) -> object;

  auto pow(object const&, object const&) -> object;

  auto numerator(object const&) -> object;

  auto denominator(object const&) -> object;

  auto floor(object const&) -> object;

  auto ceil(object const&) -> object;

  auto trunc(object const&) -> object;

  auto round(object const&) -> object;

  auto sin(object const&) -> object;

  auto cos(object const&) -> object;

  auto tan(object const&) -> object;

  auto asin(object const&) -> object;

  auto acos(object const&) -> object;

  auto atan(object const&) -> object;

  auto atan(object const&, object const&) -> object;

  auto sinh(object const&) -> object;

  auto cosh(object const&) -> object;

  auto tanh(object const&) -> object;

  auto asinh(object const&) -> object;

  auto acosh(object const&) -> object;

  auto atanh(object const&) -> object;

  auto exp(object const&) -> object;

  auto log(object const&) -> object;

  auto gamma(object const&) -> object;

  auto next_after(object const&, object const&) -> object;

  auto copy_sign(object const&, object const&) -> object;

  auto load_exponent(object const&, object const&) -> object;

  auto number_to_string(object const&, int) -> object;
} // namespace number
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
