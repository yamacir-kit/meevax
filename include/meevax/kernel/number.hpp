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

#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <meevax/iostream/concatenate.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/ratio.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/type_index.hpp>

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
  auto operator % (exact_integer const&, complex const&) -> complex;
  auto operator ==(exact_integer const&, complex const&) -> bool;
  auto operator !=(exact_integer const&, complex const&) -> bool;
  auto operator < (exact_integer const&, complex const&) -> bool;
  auto operator <=(exact_integer const&, complex const&) -> bool;
  auto operator > (exact_integer const&, complex const&) -> bool;
  auto operator >=(exact_integer const&, complex const&) -> bool;

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
  auto operator % (ratio const&, complex const&) -> complex;
  auto operator ==(ratio const&, complex const&) -> bool;
  auto operator !=(ratio const&, complex const&) -> bool;
  auto operator < (ratio const&, complex const&) -> bool;
  auto operator <=(ratio const&, complex const&) -> bool;
  auto operator > (ratio const&, complex const&) -> bool;
  auto operator >=(ratio const&, complex const&) -> bool;

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
  auto operator % (float, complex const&) -> complex;
  auto operator ==(float, complex const&) -> bool;
  auto operator !=(float, complex const&) -> bool;
  auto operator < (float, complex const&) -> bool;
  auto operator <=(float, complex const&) -> bool;
  auto operator > (float, complex const&) -> bool;
  auto operator >=(float, complex const&) -> bool;

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
  auto operator % (double, complex const&) -> complex;
  auto operator ==(double, complex const&) -> bool;
  auto operator !=(double, complex const&) -> bool;
  auto operator < (double, complex const&) -> bool;
  auto operator <=(double, complex const&) -> bool;
  auto operator > (double, complex const&) -> bool;
  auto operator >=(double, complex const&) -> bool;

  auto operator + (complex const&, complex const&) -> complex;
  auto operator - (complex const&, complex const&) -> complex;
  auto operator * (complex const&, complex const&) -> complex;
  auto operator / (complex const&, complex const&) -> complex;
  auto operator % (complex const&, complex const&) -> complex;
  auto operator ==(complex const&, complex const&) -> bool;
  auto operator !=(complex const&, complex const&) -> bool;
  auto operator < (complex const&, complex const&) -> bool;
  auto operator <=(complex const&, complex const&) -> bool;
  auto operator > (complex const&, complex const&) -> bool;
  auto operator >=(complex const&, complex const&) -> bool;

  auto operator + (complex const&, float) -> complex;
  auto operator - (complex const&, float) -> complex;
  auto operator * (complex const&, float) -> complex;
  auto operator / (complex const&, float) -> complex;
  auto operator % (complex const&, float) -> complex;
  auto operator ==(complex const&, float) -> bool;
  auto operator !=(complex const&, float) -> bool;
  auto operator < (complex const&, float) -> bool;
  auto operator <=(complex const&, float) -> bool;
  auto operator > (complex const&, float) -> bool;
  auto operator >=(complex const&, float) -> bool;

  auto operator + (complex const&, double) -> complex;
  auto operator - (complex const&, double) -> complex;
  auto operator * (complex const&, double) -> complex;
  auto operator / (complex const&, double) -> complex;
  auto operator % (complex const&, double) -> complex;
  auto operator ==(complex const&, double) -> bool;
  auto operator !=(complex const&, double) -> bool;
  auto operator < (complex const&, double) -> bool;
  auto operator <=(complex const&, double) -> bool;
  auto operator > (complex const&, double) -> bool;
  auto operator >=(complex const&, double) -> bool;

  auto operator + (complex const&, ratio const&) -> complex;
  auto operator - (complex const&, ratio const&) -> complex;
  auto operator * (complex const&, ratio const&) -> complex;
  auto operator / (complex const&, ratio const&) -> complex;
  auto operator % (complex const&, ratio const&) -> complex;
  auto operator ==(complex const&, ratio const&) -> bool;
  auto operator !=(complex const&, ratio const&) -> bool;
  auto operator < (complex const&, ratio const&) -> bool;
  auto operator <=(complex const&, ratio const&) -> bool;
  auto operator > (complex const&, ratio const&) -> bool;
  auto operator >=(complex const&, ratio const&) -> bool;

  auto operator + (complex const&, exact_integer const&) -> complex;
  auto operator - (complex const&, exact_integer const&) -> complex;
  auto operator * (complex const&, exact_integer const&) -> complex;
  auto operator / (complex const&, exact_integer const&) -> complex;
  auto operator % (complex const&, exact_integer const&) -> complex;
  auto operator ==(complex const&, exact_integer const&) -> bool;
  auto operator !=(complex const&, exact_integer const&) -> bool;
  auto operator < (complex const&, exact_integer const&) -> bool;
  auto operator <=(complex const&, exact_integer const&) -> bool;
  auto operator > (complex const&, exact_integer const&) -> bool;
  auto operator >=(complex const&, exact_integer const&) -> bool;

  auto operator + (const_reference, const_reference) -> value_type;
  auto operator - (const_reference, const_reference) -> value_type;
  auto operator * (const_reference, const_reference) -> value_type;
  auto operator / (const_reference, const_reference) -> value_type;
  auto operator % (const_reference, const_reference) -> value_type;

  using plus = std::plus<void>;

  using minus = std::minus<void>;

  using multiplies = std::multiplies<void>;

  using divides = std::divides<void>;

  struct modulus
  {
    template <typename T, typename U>
    auto operator ()(T&& x, U&& y) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>> and
                    std::is_floating_point_v<std::decay_t<U>>)
      {
        return std::fmod(x, y);
      }
      else
      {
        return x % y;
      }
    }
  };

  struct equal_to
  {
    template <typename T, typename U>
    auto operator ()(T&& x, U&& y) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>> and
                    std::is_floating_point_v<std::decay_t<U>>)
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
          return std::abs(x - y) <= std::numeric_limits<decltype(std::declval<T>() - std::declval<U>())>::epsilon();
        }
      }
      else
      {
        return x == y;
      }
    }
  };

  using less = std::less<void>;

  using less_equal = std::less_equal<void>;

  using greater = std::greater<void>;

  using greater_equal = std::greater_equal<void>;

  template <typename F, typename... Ts>
  struct application
  {
    static inline constexpr F f {};

    template <typename T>
    auto finish(T&& x) -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<T>, value_type>)
      {
        return std::forward<decltype(x)>(x);
      }
      else if constexpr (std::is_same_v<std::decay_t<T>, complex>)
      {
        if (x.imag() == e0)
        {
          return make(x.real());
        }
        else
        {
          return make(std::forward<decltype(x)>(x));
        }
      }
      else if constexpr (std::is_same_v<std::decay_t<T>, ratio>)
      {
        if (x.denominator() == 1)
        {
          return make(x.numerator());
        }
        else
        {
          return make(std::forward<decltype(x)>(x));
        }
      }
      else
      {
        return make(std::forward<decltype(x)>(x));
      }
    }

    auto operator ()(const_reference x) -> value_type
    {
      return finish(f(x.as<std::tuple_element_t<0, std::tuple<Ts...>>>()));
    }

    auto operator ()(const_reference x, const_reference y) -> value_type
    {
      return finish(f(x.as<std::tuple_element_t<0, std::tuple<Ts...>>>(),
                      y.as<std::tuple_element_t<1, std::tuple<Ts...>>>()));
    }
  };

  template <typename F>
  auto apply(const_reference x) -> value_type
  {
    static const std::unordered_map<
      type_index<1>,
      std::function<value_type (const_reference)>
    > apply
    {
      { type_index<1>(typeid(exact_integer)), application<F, exact_integer>() },
      { type_index<1>(typeid(ratio        )), application<F, ratio        >() },
      { type_index<1>(typeid(float        )), application<F, float        >() },
      { type_index<1>(typeid(double       )), application<F, double       >() },
      { type_index<1>(typeid(complex      )), application<F, complex      >() },
    };

    return apply.at(type_index<1>(x.type()))(x);
  }

  template <typename F>
  auto apply(const_reference x, const_reference y) -> value_type
  {
    #define APPLY(T, U) { type_index<2>(typeid(T), typeid(U)), application<F, T, U>() }

    static const std::unordered_map<
      type_index<2>,
      std::function<value_type (const_reference, const_reference)>
    > apply
    {
      APPLY(exact_integer, exact_integer), APPLY(exact_integer, ratio), APPLY(exact_integer, float), APPLY(exact_integer, double), APPLY(exact_integer, complex),
      APPLY(ratio,         exact_integer), APPLY(ratio,         ratio), APPLY(ratio,         float), APPLY(ratio,         double), APPLY(ratio,         complex),
      APPLY(float,         exact_integer), APPLY(float,         ratio), APPLY(float,         float), APPLY(float,         double), APPLY(float,         complex),
      APPLY(double,        exact_integer), APPLY(double,        ratio), APPLY(double,        float), APPLY(double,        double), APPLY(double,        complex),
      APPLY(complex,       exact_integer), APPLY(complex,       ratio), APPLY(complex,       float), APPLY(complex,       double), APPLY(complex,       complex),
    };

    #undef APPLY

    return apply.at(type_index<2>(x.type(), y.type()))(x, y);
  }

  struct exact
  {
    template <typename T>
    auto operator ()(T&& x) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<T>, complex>)
      {
        return complex(apply<exact>(x.real()),
                       apply<exact>(x.imag()));
      }
      else if constexpr (std::is_floating_point_v<std::decay_t<T>>)
      {
        return ratio(std::forward<decltype(x)>(x));
      }
      else
      {
        return std::forward<decltype(x)>(x);
      }
    }
  }
  inline constexpr exact_cast;

  struct inexact
  {
    template <typename T>
    auto operator ()(T&& x) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)
      {
        return complex(apply<inexact>(x.real()),
                       apply<inexact>(x.imag()));
      }
      else if constexpr (std::is_floating_point_v<std::decay_t<decltype(x)>>)
      {
        return std::forward<decltype(x)>(x);
      }
      else
      {
        return static_cast<double>(std::forward<decltype(x)>(x));
      }
    }
  }
  inline constexpr inexact_cast;

  struct is_complex
  {
    template <typename T>
    constexpr auto operator ()(T&&) const
    {
      return true;
    }
  };

  struct is_real
  {
    template <typename T>
    constexpr auto operator ()(T&&) const
    {
      return not std::is_same_v<std::decay_t<T>, complex>;
    }
  };

  struct is_rational
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>>)
      {
        return not std::isnan(x) and not std::isinf(x);
      }
      else
      {
        return std::is_same_v<std::decay_t<T>, exact_integer> or
               std::is_same_v<std::decay_t<T>, ratio>;
      }
    }
  };

  struct is_integer
  {
    template <typename T>
    auto operator ()(T&& x) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>>)
      {
        return x == std::trunc(x);
      }
      else if constexpr (std::is_same_v<std::decay_t<T>, ratio>)
      {
        return x.denominator() == 1;
      }
      else
      {
        return std::is_same_v<std::decay_t<T>, exact_integer>;
      }
    }
  };

  struct is_infinite
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)
      {
        return apply<is_infinite>(x.real()) or apply<is_infinite>(x.imag());
      }
      else if constexpr (std::is_floating_point_v<std::decay_t<T>>)
      {
        return std::isinf(x);
      }
      else
      {
        return false;
      }
    }
  };

  struct is_finite
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const
    {
      return not std::invoke(is_infinite(), std::forward<decltype(x)>(x));
    }
  };

  struct is_nan
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)
      {
        return apply<is_nan>(x.real()) or apply<is_nan>(x.imag());
      }
      else if constexpr (std::is_floating_point_v<std::decay_t<T>>)
      {
        return std::isnan(x);
      }
      else
      {
        return false;
      }
    }
  };

  struct sqrt
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)
      {
        return std::sqrt(static_cast<std::complex<double>>(std::forward<decltype(x)>(x)));
      }
      else if constexpr (std::is_same_v<std::decay_t<decltype(x)>, exact_integer>)
      {
        if (auto&& [s, r] = exact_integer_sqrt(x); r == 0)
        {
          return make(s);
        }
        else
        {
          return make(std::sqrt(inexact_cast(x)));
        }
      }
      else
      {
        return std::sqrt(inexact_cast(x));
      }
    }
  };

  struct expt
  {
    template <typename Base, typename Exponent>
    auto operator ()(Base&& base, Exponent&& exponent) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(base)>, complex> or
                    std::is_same_v<std::decay_t<decltype(exponent)>, complex>)
      {
        throw std::invalid_argument("unsupported operation"); // TODO
        return e0;
      }
      else if constexpr (std::is_same_v<std::decay_t<decltype(base)>, exact_integer> and
                         std::is_same_v<std::decay_t<decltype(exponent)>, exact_integer>)
      {
        exact_integer result {};
        mpz_pow_ui(result.value, base.value, static_cast<unsigned long>(exponent));
        return result;
      }
      else
      {
        return std::pow(inexact_cast(std::forward<decltype(base)>(base)),
                        inexact_cast(std::forward<decltype(exponent)>(exponent)));
      }
    }
  };

  struct atan2
  {
    template <typename T, typename U>
    auto operator ()(T&& x, U&& y) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex> or
                    std::is_same_v<std::decay_t<decltype(y)>, complex>)
      {
        throw std::invalid_argument("unsupported operation");
        return e0; // dummy return value.
      }
      else
      {
        return std::atan2(inexact_cast(std::forward<decltype(x)>(x)),
                          inexact_cast(std::forward<decltype(y)>(y)));
      }
    }
  };

  #define DEFINE(ROUND)                                                        \
  struct ROUND                                                                 \
  {                                                                            \
    template <typename T>                                                      \
    constexpr auto operator ()(T&& x) const                                    \
    {                                                                          \
      if constexpr (std::is_floating_point_v<std::decay_t<decltype(x)>>)       \
      {                                                                        \
        return std::ROUND(inexact_cast(x));                                    \
      }                                                                        \
      else if constexpr (std::is_same_v<std::decay_t<decltype(x)>, ratio>)     \
      {                                                                        \
        return exact_integer(std::ROUND(inexact_cast(x)));                     \
      }                                                                        \
      else if constexpr (std::is_same_v<std::decay_t<decltype(x)>, exact_integer>) \
      {                                                                        \
        return std::forward<decltype(x)>(x);                                   \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return complex(apply<ROUND>(x.real()),                                 \
                       apply<ROUND>(x.imag()));                                \
      }                                                                        \
    }                                                                          \
  }

  DEFINE(floor);
  DEFINE(ceil);
  DEFINE(trunc);
  DEFINE(round);

  #undef DEFINE

  #define DEFINE(CMATH)                                                        \
  struct CMATH                                                                 \
  {                                                                            \
    template <typename T>                                                      \
    auto operator ()(T&& x) const                                              \
    {                                                                          \
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)        \
      {                                                                        \
        auto result = std::CMATH(static_cast<std::complex<double>>(std::forward<decltype(x)>(x))); \
        return complex(make(result.real()),                                    \
                       make(result.imag()));                                   \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return std::CMATH(inexact_cast(std::forward<decltype(x)>(x)));         \
      }                                                                        \
    }                                                                          \
  }

  DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh);
  DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh);
  DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh);

  DEFINE(exp);
  DEFINE(log);

  #undef DEFINE

  template <auto Radix>
  struct number_to_string
  {
    template <typename T>
    auto operator ()(T&& z) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>>)
      {
        return string("TODO");
      }
      else if constexpr (std::is_same_v<std::decay_t<T>, exact_integer>)
      {
        auto free = [](char * data)
        {
          void (*free)(void *, std::size_t);
          mp_get_memory_functions(nullptr, nullptr, &free);
          std::invoke(free, static_cast<void *>(data), std::strlen(data) + 1);
        };

        return string(std::unique_ptr<char, decltype(free)>(mpz_get_str(nullptr, Radix, z.value), free).get());
      }
      else
      {
        return string("TODO");
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
