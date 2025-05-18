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
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/large_integer.hpp>
#include <meevax/kernel/ratio.hpp>
#include <meevax/utility/combination.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator * (std::int64_t, large_integer const&) -> large_integer;
  auto operator + (std::int64_t, large_integer const&) -> large_integer;
  auto operator - (std::int64_t, large_integer const&) -> large_integer;
  auto operator / (std::int64_t, large_integer const&) -> ratio;
  auto operator % (std::int64_t, large_integer const&) -> large_integer;
  auto operator !=(std::int64_t, large_integer const&) -> bool;
  auto operator < (std::int64_t, large_integer const&) -> bool;
  auto operator <=(std::int64_t, large_integer const&) -> bool;
  auto operator ==(std::int64_t, large_integer const&) -> bool;
  auto operator > (std::int64_t, large_integer const&) -> bool;
  auto operator >=(std::int64_t, large_integer const&) -> bool;

  auto operator * (std::int64_t, ratio const&) -> ratio;
  auto operator + (std::int64_t, ratio const&) -> ratio;
  auto operator - (std::int64_t, ratio const&) -> ratio;
  auto operator / (std::int64_t, ratio const&) -> ratio;
  auto operator % (std::int64_t, ratio const&) -> ratio;
  auto operator !=(std::int64_t, ratio const&) -> bool;
  auto operator < (std::int64_t, ratio const&) -> bool;
  auto operator <=(std::int64_t, ratio const&) -> bool;
  auto operator ==(std::int64_t, ratio const&) -> bool;
  auto operator > (std::int64_t, ratio const&) -> bool;
  auto operator >=(std::int64_t, ratio const&) -> bool;

  auto operator * (std::int64_t, complex const&) -> complex;
  auto operator + (std::int64_t, complex const&) -> complex;
  auto operator - (std::int64_t, complex const&) -> complex;
  auto operator / (std::int64_t, complex const&) -> complex;
  auto operator ==(std::int64_t, complex const&) -> bool;
  auto operator !=(std::int64_t, complex const&) -> bool;

  auto operator * (large_integer const&, std::int64_t) -> large_integer;
  auto operator + (large_integer const&, std::int64_t) -> large_integer;
  auto operator - (large_integer const&, std::int64_t) -> large_integer;
  auto operator / (large_integer const&, std::int64_t) -> ratio;
  auto operator % (large_integer const&, std::int64_t) -> large_integer;
  auto operator !=(large_integer const&, std::int64_t) -> bool;
  auto operator < (large_integer const&, std::int64_t) -> bool;
  auto operator <=(large_integer const&, std::int64_t) -> bool;
  auto operator ==(large_integer const&, std::int64_t) -> bool;
  auto operator > (large_integer const&, std::int64_t) -> bool;
  auto operator >=(large_integer const&, std::int64_t) -> bool;

  auto operator * (large_integer const&, large_integer const&) -> large_integer;
  auto operator + (large_integer const&, large_integer const&) -> large_integer;
  auto operator - (large_integer const&, large_integer const&) -> large_integer;
  auto operator / (large_integer const&, large_integer const&) -> ratio;
  auto operator % (large_integer const&, large_integer const&) -> large_integer;
  auto operator !=(large_integer const&, large_integer const&) -> bool;
  auto operator < (large_integer const&, large_integer const&) -> bool;
  auto operator <=(large_integer const&, large_integer const&) -> bool;
  auto operator ==(large_integer const&, large_integer const&) -> bool;
  auto operator > (large_integer const&, large_integer const&) -> bool;
  auto operator >=(large_integer const&, large_integer const&) -> bool;

  auto operator * (large_integer const&, ratio const&) -> ratio;
  auto operator + (large_integer const&, ratio const&) -> ratio;
  auto operator - (large_integer const&, ratio const&) -> ratio;
  auto operator / (large_integer const&, ratio const&) -> ratio;
  auto operator % (large_integer const&, ratio const&) -> ratio;
  auto operator !=(large_integer const&, ratio const&) -> bool;
  auto operator < (large_integer const&, ratio const&) -> bool;
  auto operator <=(large_integer const&, ratio const&) -> bool;
  auto operator ==(large_integer const&, ratio const&) -> bool;
  auto operator > (large_integer const&, ratio const&) -> bool;
  auto operator >=(large_integer const&, ratio const&) -> bool;

  auto operator * (large_integer const&, float) -> float;
  auto operator + (large_integer const&, float) -> float;
  auto operator - (large_integer const&, float) -> float;
  auto operator / (large_integer const&, float) -> float;
  auto operator % (large_integer const&, float) -> float;
  auto operator !=(large_integer const&, float) -> bool;
  auto operator < (large_integer const&, float) -> bool;
  auto operator <=(large_integer const&, float) -> bool;
  auto operator ==(large_integer const&, float) -> bool;
  auto operator > (large_integer const&, float) -> bool;
  auto operator >=(large_integer const&, float) -> bool;

  auto operator * (large_integer const&, double) -> double;
  auto operator + (large_integer const&, double) -> double;
  auto operator - (large_integer const&, double) -> double;
  auto operator / (large_integer const&, double) -> double;
  auto operator % (large_integer const&, double) -> double;
  auto operator !=(large_integer const&, double) -> bool;
  auto operator < (large_integer const&, double) -> bool;
  auto operator <=(large_integer const&, double) -> bool;
  auto operator ==(large_integer const&, double) -> bool;
  auto operator > (large_integer const&, double) -> bool;
  auto operator >=(large_integer const&, double) -> bool;

  auto operator * (large_integer const&, complex const&) -> complex;
  auto operator + (large_integer const&, complex const&) -> complex;
  auto operator - (large_integer const&, complex const&) -> complex;
  auto operator / (large_integer const&, complex const&) -> complex;
  auto operator ==(large_integer const&, complex const&) -> bool;
  auto operator !=(large_integer const&, complex const&) -> bool;

  auto operator * (ratio const&, std::int64_t) -> ratio;
  auto operator + (ratio const&, std::int64_t) -> ratio;
  auto operator - (ratio const&, std::int64_t) -> ratio;
  auto operator / (ratio const&, std::int64_t) -> ratio;
  auto operator % (ratio const&, std::int64_t) -> ratio;
  auto operator !=(ratio const&, std::int64_t) -> bool;
  auto operator < (ratio const&, std::int64_t) -> bool;
  auto operator <=(ratio const&, std::int64_t) -> bool;
  auto operator ==(ratio const&, std::int64_t) -> bool;
  auto operator > (ratio const&, std::int64_t) -> bool;
  auto operator >=(ratio const&, std::int64_t) -> bool;

  auto operator * (ratio const&, large_integer const&) -> ratio;
  auto operator + (ratio const&, large_integer const&) -> ratio;
  auto operator - (ratio const&, large_integer const&) -> ratio;
  auto operator / (ratio const&, large_integer const&) -> ratio;
  auto operator % (ratio const&, large_integer const&) -> ratio;
  auto operator !=(ratio const&, large_integer const&) -> bool;
  auto operator < (ratio const&, large_integer const&) -> bool;
  auto operator <=(ratio const&, large_integer const&) -> bool;
  auto operator ==(ratio const&, large_integer const&) -> bool;
  auto operator > (ratio const&, large_integer const&) -> bool;
  auto operator >=(ratio const&, large_integer const&) -> bool;

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

  auto operator + (float, large_integer const&) -> float;
  auto operator - (float, large_integer const&) -> float;
  auto operator * (float, large_integer const&) -> float;
  auto operator / (float, large_integer const&) -> float;
  auto operator % (float, large_integer const&) -> float;
  auto operator ==(float, large_integer const&) -> bool;
  auto operator !=(float, large_integer const&) -> bool;
  auto operator < (float, large_integer const&) -> bool;
  auto operator <=(float, large_integer const&) -> bool;
  auto operator > (float, large_integer const&) -> bool;
  auto operator >=(float, large_integer const&) -> bool;

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

  auto operator + (double, large_integer const&) -> double;
  auto operator - (double, large_integer const&) -> double;
  auto operator * (double, large_integer const&) -> double;
  auto operator / (double, large_integer const&) -> double;
  auto operator % (double, large_integer const&) -> double;
  auto operator ==(double, large_integer const&) -> bool;
  auto operator !=(double, large_integer const&) -> bool;
  auto operator < (double, large_integer const&) -> bool;
  auto operator <=(double, large_integer const&) -> bool;
  auto operator > (double, large_integer const&) -> bool;
  auto operator >=(double, large_integer const&) -> bool;

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

  auto operator + (complex const&, std::int64_t) -> complex;
  auto operator - (complex const&, std::int64_t) -> complex;
  auto operator * (complex const&, std::int64_t) -> complex;
  auto operator / (complex const&, std::int64_t) -> complex;
  auto operator ==(complex const&, std::int64_t) -> bool;
  auto operator !=(complex const&, std::int64_t) -> bool;

  auto operator + (complex const&, large_integer const&) -> complex;
  auto operator - (complex const&, large_integer const&) -> complex;
  auto operator * (complex const&, large_integer const&) -> complex;
  auto operator / (complex const&, large_integer const&) -> complex;
  auto operator ==(complex const&, large_integer const&) -> bool;
  auto operator !=(complex const&, large_integer const&) -> bool;

  auto operator + (complex const&, ratio const&) -> complex;
  auto operator - (complex const&, ratio const&) -> complex;
  auto operator * (complex const&, ratio const&) -> complex;
  auto operator / (complex const&, ratio const&) -> complex;
  auto operator ==(complex const&, ratio const&) -> bool;
  auto operator !=(complex const&, ratio const&) -> bool;

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

  auto operator + (complex const&, complex const&) -> complex;
  auto operator - (complex const&, complex const&) -> complex;
  auto operator * (complex const&, complex const&) -> complex;
  auto operator / (complex const&, complex const&) -> complex;
  auto operator ==(complex const&, complex const&) -> bool;
  auto operator !=(complex const&, complex const&) -> bool;

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

  inline auto inexact_equals = []<typename T, typename U>(T const& x, U const& y)
  {
    if constexpr (std::is_floating_point_v<T> and std::is_floating_point_v<U>)
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
  };

  template <typename T>
  auto exact_integer_cast(object const& x)
  {
    if (auto const& t = x.type(); t == typeid(std::int32_t))
    {
      return static_cast<T>(x.as<std::int32_t>());
    }
    else if (t == typeid(large_integer))
    {
      return static_cast<T>(x.as<large_integer>());
    }
    else if (t == typeid(ratio))
    {
      return static_cast<T>(x.as<ratio>());
    }
    else
    {
      throw error(make<string>("not an exact integer"), x);
    }
  }

inline namespace number
{
  using complex_number = std::tuple<std::int32_t, large_integer, ratio, float, double, complex>;

  using complex_numbers = combination<std::int32_t, large_integer, ratio, float, double, complex>;

  using real_number = std::tuple<std::int32_t, large_integer, ratio, float, double>;

  using real_numbers = combination<std::int32_t, large_integer, ratio, float, double>;

  using exact_integers = combination<std::int32_t, large_integer>;

  template <typename T>
  auto canonicalize(T&& x) -> decltype(auto)
  {
    if constexpr (std::is_same_v<std::decay_t<T>, object> or
                  std::is_same_v<std::decay_t<T>, object::pointer>)
    {
      return std::forward<decltype(x)>(x);
    }
    else if constexpr (std::is_same_v<std::decay_t<T>, complex>)
    {
      return x.canonicalize();
    }
    else if constexpr (std::is_same_v<std::decay_t<T>, ratio>)
    {
      return x.denominator() == 1_i64 ? make(x.numerator()) : make(std::forward<decltype(x)>(x));
    }
    else if constexpr (std::is_same_v<std::decay_t<T>, std::int64_t>)
    {
      if (std::numeric_limits<std::int32_t>::min() <= x and x <= std::numeric_limits<std::int32_t>::max())
      {
        return make(static_cast<std::int32_t>(x));
      }
      else
      {
        return make<large_integer>(x);
      }
    }
    else
    {
      return make(std::forward<decltype(x)>(x));
    }
  }

  template <typename T>
  auto widen(T&& x) -> decltype(auto)
  {
    if constexpr (std::is_same_v<T, std::int32_t>)
    {
      return static_cast<std::int64_t>(std::forward<decltype(x)>(x));
    }
    else
    {
      return std::forward<decltype(x)>(x);
    }
  }

  template <typename Tuple, auto I = 0, typename F>
  auto apply_to([[maybe_unused]] F f, object const& x) -> object
  {
    if constexpr (I < std::tuple_size_v<Tuple>)
    {
      using type_i = std::tuple_element_t<I, Tuple>;

      if (x.is<type_i>())
      {
        return canonicalize(f(widen(x.as<type_i>())));
      }
      else
      {
        return apply_to<Tuple, I + 1>(f, x);
      }
    }
    else
    {
      throw error(make<string>("not an number"));
    }
  }

  template <typename Tuple, auto I = 0, typename F>
  auto apply_to([[maybe_unused]] F f, object const& x, object const& y) -> object
  {
    if constexpr (I < std::tuple_size_v<Tuple>)
    {
      using type_i_0 = std::tuple_element_t<0, std::tuple_element_t<I, Tuple>>;
      using type_i_1 = std::tuple_element_t<1, std::tuple_element_t<I, Tuple>>;

      if (x.is<type_i_0>() and y.is<type_i_1>())
      {
        return canonicalize(f(widen(x.as<type_i_0>()),
                              widen(y.as<type_i_1>())));
      }
      else
      {
        return apply_to<Tuple, I + 1>(f, x, y);
      }
    }
    else
    {
      throw error(make<string>("not an number"));
    }
  }

  template <typename Tuple, auto I = 0, typename F>
  auto test([[maybe_unused]] F f, object const& x) -> bool
  {
    if constexpr (I < std::tuple_size_v<Tuple>)
    {
      using type_i = std::tuple_element_t<I, Tuple>;

      if (x.is<type_i>())
      {
        return f(widen(x.as<type_i>()));
      }
      else
      {
        return test<Tuple, I + 1>(f, x);
      }
    }
    else
    {
      return false;
    }
  }

  template <typename Tuple, auto I = 0, typename F>
  auto test([[maybe_unused]] F f, object const& x, object const& y) -> bool
  {
    if constexpr (I < std::tuple_size_v<Tuple>)
    {
      using type_i_0 = std::tuple_element_t<0, std::tuple_element_t<I, Tuple>>;
      using type_i_1 = std::tuple_element_t<1, std::tuple_element_t<I, Tuple>>;

      if (x.is<type_i_0>() and y.is<type_i_1>())
      {
        return f(widen(x.as<type_i_0>()),
                 widen(y.as<type_i_1>()));
      }
      else
      {
        return test<Tuple, I + 1>(f, x, y);
      }
    }
    else
    {
      return false;
    }
  }

  auto equals(object const&, object const&) -> bool;

  auto exact_integer_equals(object const&, object const&) -> bool;

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

  auto abs(object const&) -> object;

  auto quotient(object const&, object const&) -> object;

  auto remainder(object const&, object const&) -> object;

  auto modulo(object const&, object const&) -> object;

  auto gcd(object const&, object const&) -> object;

  auto lcm(object const&, object const&) -> object;

  auto sqrt(object const&) -> object;

  auto pow(object const&, object const&) -> object;

  auto real(object const&) -> object;

  auto imag(object const&) -> object;

  auto magnitude(object const&) -> object;

  auto angle(object const&) -> object;

  auto numerator(object const&) -> object;

  auto denominator(object const&) -> object;

  auto ldexp(object const&, object const&) -> object;

  auto number_to_string(object const&, int) -> object;

  auto floor(object const&) -> object;

  auto ceiling(object const&) -> object;

  auto truncate(object const&) -> object;

  auto round(object const&) -> object;

  auto sin(object const&) -> object;

  auto cos(object const&) -> object;

  auto tan(object const&) -> object;

  auto asin(object const&) -> object;

  auto acos(object const&) -> object;

  auto atan(object const&) -> object;

  auto atan2(object const&, object const&) -> object;

  auto sinh(object const&) -> object;

  auto cosh(object const&) -> object;

  auto tanh(object const&) -> object;

  auto asinh(object const&) -> object;

  auto acosh(object const&) -> object;

  auto atanh(object const&) -> object;

  auto exp(object const&) -> object;

  auto log(object const&) -> object;

  auto fabs(object const&) -> object;

  auto expm1(object const&) -> object;

  auto log1p(object const&) -> object;

  auto tgamma(object const&) -> object;

  auto lgamma(object const&) -> object;

  auto erf(object const&) -> object;

  auto erfc(object const&) -> object;

  auto copysign(object const&, object const&) -> object;

  auto nextafter(object const&, object const&) -> object;

  auto cyl_bessel_j(object const&, object const&) -> object;

  auto cyl_neumann(object const&, object const&) -> object;
} // namespace number
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
