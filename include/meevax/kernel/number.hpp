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
#include <meevax/kernel/floating_point.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/ratio.hpp>
#include <meevax/kernel/type_index.hpp>

namespace meevax
{
inline namespace kernel
{
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
      { type_index<1>(typeid(single_float )), application<F, single_float >() },
      { type_index<1>(typeid(double_float )), application<F, double_float >() },
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
      APPLY(exact_integer, exact_integer), APPLY(exact_integer, ratio), APPLY(exact_integer, single_float), APPLY(exact_integer, double_float), APPLY(exact_integer, float), APPLY(exact_integer, double),
      APPLY(ratio,         exact_integer), APPLY(ratio,         ratio), APPLY(ratio,         single_float), APPLY(ratio,         double_float), APPLY(ratio,         float), APPLY(ratio,         double),
      APPLY(single_float,  exact_integer), APPLY(single_float,  ratio), APPLY(single_float,  single_float), APPLY(single_float,  double_float),
      APPLY(double_float,  exact_integer), APPLY(double_float,  ratio), APPLY(double_float,  single_float), APPLY(double_float,  double_float),
      APPLY(float,         exact_integer), APPLY(float,         ratio),
      APPLY(double,        exact_integer), APPLY(double,        ratio),
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

  struct inexact  // TODO REMOVE THIS!!!
  {
    template <typename T>
    auto operator ()(T const& x) const -> decltype(auto)
    {
      if constexpr (std::is_floating_point_v<std::decay_t<decltype(x)>>)
      {
        return floating_point(std::forward<decltype(x)>(x));
      }
      else
      {
        return floating_point(static_cast<double>(std::forward<decltype(x)>(x)));
      }
    }
  };

  struct native_inexact
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
  }
  inline constexpr inexact_cast;

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

  template <typename T> auto operator * (exact_integer const& a, floating_point<T> const& b)         { return inexact()(a) *  b; }
  template <typename T> auto operator + (exact_integer const& a, floating_point<T> const& b)         { return inexact()(a) +  b; }
  template <typename T> auto operator - (exact_integer const& a, floating_point<T> const& b)         { return inexact()(a) -  b; }
  template <typename T> auto operator / (exact_integer const& a, floating_point<T> const& b)         { return inexact()(a) /  b; }
  template <typename T> auto operator % (exact_integer const& a, floating_point<T> const& b)         { return inexact()(a) %  b; }
  template <typename T> auto operator !=(exact_integer const& a, floating_point<T> const& b) -> bool { return inexact()(a) != b; }
  template <typename T> auto operator < (exact_integer const& a, floating_point<T> const& b) -> bool { return inexact()(a) <  b; }
  template <typename T> auto operator <=(exact_integer const& a, floating_point<T> const& b) -> bool { return inexact()(a) <= b; }
  template <typename T> auto operator ==(exact_integer const& a, floating_point<T> const& b) -> bool { return inexact()(a) == b; }
  template <typename T> auto operator > (exact_integer const& a, floating_point<T> const& b) -> bool { return inexact()(a) >  b; }
  template <typename T> auto operator >=(exact_integer const& a, floating_point<T> const& b) -> bool { return inexact()(a) >= b; }

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

  template <typename T> auto operator * (ratio const& a, floating_point<T> const& b)         { return inexact()(a) *  b; }
  template <typename T> auto operator + (ratio const& a, floating_point<T> const& b)         { return inexact()(a) +  b; }
  template <typename T> auto operator - (ratio const& a, floating_point<T> const& b)         { return inexact()(a) -  b; }
  template <typename T> auto operator / (ratio const& a, floating_point<T> const& b)         { return inexact()(a) /  b; }
  template <typename T> auto operator % (ratio const& a, floating_point<T> const& b)         { return inexact()(a) %  b; }
  template <typename T> auto operator !=(ratio const& a, floating_point<T> const& b) -> bool { return inexact()(a) != b; }
  template <typename T> auto operator < (ratio const& a, floating_point<T> const& b) -> bool { return inexact()(a) <  b; }
  template <typename T> auto operator <=(ratio const& a, floating_point<T> const& b) -> bool { return inexact()(a) <= b; }
  template <typename T> auto operator ==(ratio const& a, floating_point<T> const& b) -> bool { return inexact()(a) == b; }
  template <typename T> auto operator > (ratio const& a, floating_point<T> const& b) -> bool { return inexact()(a) >  b; }
  template <typename T> auto operator >=(ratio const& a, floating_point<T> const& b) -> bool { return inexact()(a) >= b; }

  template <typename T> auto operator * (floating_point<T> const& a, exact_integer const& b)         { return a *  inexact()(b); }
  template <typename T> auto operator + (floating_point<T> const& a, exact_integer const& b)         { return a +  inexact()(b); }
  template <typename T> auto operator - (floating_point<T> const& a, exact_integer const& b)         { return a -  inexact()(b); }
  template <typename T> auto operator / (floating_point<T> const& a, exact_integer const& b)         { return a /  inexact()(b); }
  template <typename T> auto operator % (floating_point<T> const& a, exact_integer const& b)         { return a %  inexact()(b); }
  template <typename T> auto operator !=(floating_point<T> const& a, exact_integer const& b) -> bool { return a != inexact()(b); }
  template <typename T> auto operator < (floating_point<T> const& a, exact_integer const& b) -> bool { return a <  inexact()(b); }
  template <typename T> auto operator <=(floating_point<T> const& a, exact_integer const& b) -> bool { return a <= inexact()(b); }
  template <typename T> auto operator ==(floating_point<T> const& a, exact_integer const& b) -> bool { return a == inexact()(b); }
  template <typename T> auto operator > (floating_point<T> const& a, exact_integer const& b) -> bool { return a >  inexact()(b); }
  template <typename T> auto operator >=(floating_point<T> const& a, exact_integer const& b) -> bool { return a >= inexact()(b); }

  template <typename T> auto operator * (floating_point<T> const& a, ratio const& b)         { return a *  inexact()(b); }
  template <typename T> auto operator + (floating_point<T> const& a, ratio const& b)         { return a +  inexact()(b); }
  template <typename T> auto operator - (floating_point<T> const& a, ratio const& b)         { return a -  inexact()(b); }
  template <typename T> auto operator / (floating_point<T> const& a, ratio const& b)         { return a /  inexact()(b); }
  template <typename T> auto operator % (floating_point<T> const& a, ratio const& b)         { return a %  inexact()(b); }
  template <typename T> auto operator !=(floating_point<T> const& a, ratio const& b) -> bool { return a != inexact()(b); }
  template <typename T> auto operator < (floating_point<T> const& a, ratio const& b) -> bool { return a <  inexact()(b); }
  template <typename T> auto operator <=(floating_point<T> const& a, ratio const& b) -> bool { return a <= inexact()(b); }
  template <typename T> auto operator ==(floating_point<T> const& a, ratio const& b) -> bool { return a == inexact()(b); }
  template <typename T> auto operator > (floating_point<T> const& a, ratio const& b) -> bool { return a >  inexact()(b); }
  template <typename T> auto operator >=(floating_point<T> const& a, ratio const& b) -> bool { return a >= inexact()(b); }

  template <typename T, typename U> auto operator * (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value * b.value); }
  template <typename T, typename U> auto operator + (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value + b.value); }
  template <typename T, typename U> auto operator - (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value - b.value); }
  template <typename T, typename U> auto operator / (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value / b.value); }
  template <typename T, typename U> auto operator % (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(std::remainder(a.value, b.value)); }
  template <typename T, typename U> auto operator ==(floating_point<T> const& a, floating_point<U> const& b) -> bool
  {
    if (std::isnan(a.value) and std::isnan(b.value))
    {
      return true;
    }
    else if (std::isinf(a.value) or std::isinf(b.value))
    {
      return a.value == b.value;
    }
    else
    {
      return std::abs(a.value - b.value) <= std::numeric_limits<decltype(std::declval<T>() - std::declval<U>())>::epsilon();
    }
  }
  template <typename T, typename U> auto operator !=(floating_point<T> const& a, floating_point<U> const& b) -> bool { return not (a == b); }
  template <typename T, typename U> auto operator < (floating_point<T> const& a, floating_point<U> const& b) -> bool { return a.value <  b.value; }
  template <typename T, typename U> auto operator <=(floating_point<T> const& a, floating_point<U> const& b) -> bool { return a.value <= b.value; }
  template <typename T, typename U> auto operator > (floating_point<T> const& a, floating_point<U> const& b) -> bool { return a.value >  b.value; }
  template <typename T, typename U> auto operator >=(floating_point<T> const& a, floating_point<U> const& b) -> bool { return a.value >= b.value; }

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

  struct is_complex
  {
    template <typename T>
    auto operator ()(T const&) const
    {
      return true;
    }
  };

  struct is_real
  {
    template <typename T>
    auto operator ()(T const&) const
    {
      return false;
    }

    template <typename T>
    auto operator ()(floating_point<T> const&) const
    {
      return true;
    }

    auto operator ()(ratio const&) const
    {
      return true;
    }

    auto operator ()(exact_integer const&) const
    {
      return true;
    }
  };

  struct is_rational
  {
    template <typename T>
    auto operator ()(T const&) const
    {
      return false;
    }

    template <typename T>
    auto operator ()(floating_point<T> const& x) const
    {
      return not std::isnan(x.value) and not std::isinf(x.value);
    }

    auto operator ()(ratio const&) const
    {
      return true;
    }

    auto operator ()(exact_integer const&) const
    {
      return true;
    }
  };

  struct is_integer
  {
    template <typename T>
    auto operator ()(T const&) const
    {
      return false;
    }

    template <typename T>
    auto operator ()(floating_point<T> const& x) const
    {
      return x.value == std::trunc(x.value);
    }

    auto operator ()(ratio const& x) const
    {
      return x.denominator().as<exact_integer>() == 1;
    }

    auto operator ()(exact_integer const&) const
    {
      return true;
    }
  };

  struct is_finite
  {
    template <typename T>
    auto operator ()(T const&) const
    {
      return true;
    }

    template <typename T>
    auto operator ()(floating_point<T> const& x) const
    {
      return not std::isinf(x.value);
    }
  };

  struct is_infinite
  {
    template <typename T>
    auto operator ()(T const&) const
    {
      return false;
    }

    template <typename T>
    auto operator ()(floating_point<T> const& x) const
    {
      return std::isinf(x.value);
    }
  };

  struct is_nan
  {
    template <typename T>
    auto operator ()(T const&) const
    {
      return false;
    }

    template <typename T>
    auto operator ()(floating_point<T> const& x) const
    {
      return std::isnan(x.value);
    }
  };

  auto exact_integer_sqrt(exact_integer const&) -> std::tuple<exact_integer, exact_integer>;

  struct sqrt
  {
    template <typename T>
    auto operator ()(T const& x) const -> decltype(auto)
    {
      return floating_point(std::sqrt(inexact()(x)));
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
      return floating_point(std::pow(inexact_cast(x), inexact_cast(y)));
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
    auto operator ()(T const& x) const -> decltype(auto)                       \
    {                                                                          \
      return floating_point(std::ROUND(inexact()(x)));                         \
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
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
