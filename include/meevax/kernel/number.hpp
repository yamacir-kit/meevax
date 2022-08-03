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

  auto operator +(const_reference, const_reference) -> value_type;
  auto operator -(const_reference, const_reference) -> value_type;
  auto operator *(const_reference, const_reference) -> value_type;
  auto operator /(const_reference, const_reference) -> value_type;
  auto operator %(const_reference, const_reference) -> value_type;

  template <typename F, typename... Ts>
  struct application
  {
    static inline constexpr F f {};

    template <typename T>
    auto finish(T&& z) -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(z)>, value_type>)
      {
        return std::forward<decltype(z)>(z);
      }
      else if constexpr (std::is_same_v<std::decay_t<decltype(z)>, ratio>)
      {
        return z.simple();
      }
      else
      {
        return make(std::forward<decltype(z)>(z));
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
      APPLY(exact_integer, exact_integer), APPLY(exact_integer, ratio), APPLY(exact_integer, float), APPLY(exact_integer, double),
      APPLY(ratio,         exact_integer), APPLY(ratio,         ratio), APPLY(ratio,         float), APPLY(ratio,         double),
      APPLY(float,         exact_integer), APPLY(float,         ratio), APPLY(float,         float), APPLY(float,         double),
      APPLY(double,        exact_integer), APPLY(double,        ratio), APPLY(double,        float), APPLY(double,        double),
    };

    #undef APPLY

    return apply.at(type_index<2>(x.type(), y.type()))(x, y);
  }

  struct exact
  {
    template <typename T>
    auto operator ()(T const& x) const -> decltype(auto)
    {
      return ratio(x).simple();
    }

    auto operator ()(exact_integer const& x) const -> auto const&
    {
      return x;
    }

    auto operator ()(ratio const& x) const -> auto const&
    {
      return x;
    }
  };

  struct inexact
  {
    template <typename T>
    auto operator ()(T const& x) const -> decltype(auto)
    {
      if constexpr (std::is_floating_point_v<std::decay_t<decltype(x)>>)
      {
        return std::forward<decltype(x)>(x);
      }
      else
      {
        return static_cast<double>(std::forward<decltype(x)>(x));
      }
    }
  } inline constexpr inexact_cast;

  struct equal_to
  {
    template <typename T, typename U>
    auto operator ()(T&& x, U&& y) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>> and std::is_floating_point_v<std::decay_t<U>>)
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

  struct modulus
  {
    template <typename T, typename U>
    auto operator ()(T&& x, U&& y) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>> and std::is_floating_point_v<std::decay_t<U>>)
      {
        return std::remainder(x, y);
      }
      else
      {
        return x % y;
      }
    }
  };

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
        return std::is_same_v<std::decay_t<T>, exact_integer> or std::is_same_v<std::decay_t<T>, ratio>;
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
        return x.denominator().template as<exact_integer>() == 1;
      }
      else
      {
        return std::is_same_v<std::decay_t<T>, exact_integer>;
      }
    }
  };

  struct is_finite
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>>)
      {
        return not std::isinf(x);
      }
      else
      {
        return true;
      }
    }
  };

  struct is_infinite
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>>)
      {
        return std::isinf(x);
      }
      else
      {
        return false;
      }
    }
  };

  struct is_nan
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>>)
      {
        return std::isnan(x);
      }
      else
      {
        return false;
      }
    }
  };

  auto exact_integer_sqrt(exact_integer const&) -> std::tuple<exact_integer, exact_integer>;

  struct sqrt
  {
    template <typename T>
    auto operator ()(T const& x) const
    {
      return std::sqrt(inexact_cast(x));
    }

    auto operator ()(exact_integer const& x) const
    {
      if (auto&& [s, r] = exact_integer_sqrt(x); r == 0)
      {
        return make(s);
      }
      else
      {
        return make(operator ()<exact_integer const&>(x));
      }
    }
  };

  struct expt
  {
    template <typename T, typename U>
    auto operator ()(T const& x, U const& y) const -> decltype(auto)
    {
      return std::pow(inexact_cast(x), inexact_cast(y));
    }

    auto operator ()(exact_integer const& base, exact_integer const& exponent) const
    {
      exact_integer result {};
      mpz_pow_ui(result.value, base.value, static_cast<unsigned long>(exponent));
      return result;
    }
  };

  #define DEFINE(ROUND)                                                        \
  struct ROUND                                                                 \
  {                                                                            \
    template <typename T>                                                      \
    auto operator ()(T const& x) const                                         \
    {                                                                          \
      return std::ROUND(inexact_cast(x));                                      \
    }                                                                          \
                                                                               \
    auto operator ()(exact_integer const& x) const -> auto const&              \
    {                                                                          \
      return x;                                                                \
    }                                                                          \
                                                                               \
    auto operator ()(ratio const& x) const                                     \
    {                                                                          \
      return exact_integer(operator ()<ratio const&>(x));                      \
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
    template <typename... Ts>                                                  \
    auto operator ()(Ts&&... xs) const                                         \
    {                                                                          \
      return std::CMATH(inexact_cast(std::forward<decltype(xs)>(xs))...);      \
    }                                                                          \
  }

  DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh);
  DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh);
  DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh);
               DEFINE(atan2);

  DEFINE(exp);
  DEFINE(log);

  #undef DEFINE
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
