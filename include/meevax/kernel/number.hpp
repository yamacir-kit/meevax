/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

  auto operator + (object const&, object const&) -> object;
  auto operator - (object const&, object const&) -> object;
  auto operator * (object const&, object const&) -> object;
  auto operator / (object const&, object const&) -> object;
  auto operator % (object const&, object const&) -> object;

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

  namespace arithmetic
  {
    template <typename T>
    auto canonicalize(T&& x) -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<T>, object>)
      {
        return std::forward<decltype(x)>(x);
      }
      else if constexpr (std::is_same_v<std::decay_t<T>, complex>)
      {
        return x.canonicalize();
      }
      else if constexpr (std::is_same_v<std::decay_t<T>, ratio>)
      {
        return x.denominator() == 1 ? make(x.numerator()) : make(std::forward<decltype(x)>(x));
      }
      else
      {
        return make(std::forward<decltype(x)>(x));
      }
    }

    template <typename F, typename... Ts>
    struct application
    {
      auto operator ()(object const& x) -> object
      {
        return canonicalize(F()(x.as<std::tuple_element_t<0, std::tuple<Ts...>>>()));
      }

      auto operator ()(object const& x, object const& y) -> object
      {
        return canonicalize(F()(x.as<std::tuple_element_t<0, std::tuple<Ts...>>>(),
                                y.as<std::tuple_element_t<1, std::tuple<Ts...>>>()));
      }
    };

    template <auto I = 0, typename F>
    auto apply([[maybe_unused]] F f, object const& x) -> object
    {
      using Ts = std::tuple<exact_integer, ratio, float, double, complex>;

      if constexpr (I < std::tuple_size_v<Ts>)
      {
        using T = std::tuple_element_t<I, Ts>;
        return x.is<T>() ? canonicalize(f(x.as<T>())) : apply<I + 1>(f, x);
      }
      else
      {
        throw std::out_of_range("not an number");
      }
    }

    template <typename...>
    struct make_combination;

    template <typename T, auto... Is>
    struct make_combination<T, std::index_sequence<Is...>>
    {
      using type = std::tuple<std::pair<typename std::tuple_element_t<Is / std::tuple_size_v<T>, T>,
                                        typename std::tuple_element_t<Is % std::tuple_size_v<T>, T>> ...>;
    };

    template <typename... Ts>
    using combination = typename make_combination<std::tuple<Ts...>, std::make_index_sequence<sizeof...(Ts) * sizeof...(Ts)>>::type;

    template <typename F, auto N>
    struct apply_t
    {
      static inline std::unordered_map<
        type_index<N>,
        std::function<object (object const&, object const&)>
      > data {};

      template <typename Tuple, auto I = 0>
      auto emplace()
      {
        if constexpr (I < std::tuple_size_v<Tuple>)
        {
          if constexpr (N == 2)
          {
            using element = std::tuple_element_t<I, Tuple>;

            data.emplace(std::piecewise_construct,
                         std::forward_as_tuple(typeid(std::tuple_element_t<0, element>),
                                               typeid(std::tuple_element_t<1, element>)),
                         std::forward_as_tuple(application<F, std::tuple_element_t<0, element>,
                                                              std::tuple_element_t<1, element>>()));
          }
          else
          {
            // TODO unary
          }

          emplace<Tuple, I + 1>();
        }
      }

      explicit apply_t()
      {
        if constexpr (N == 2)
        {
          emplace<combination<exact_integer, ratio, float, double, complex>>();
        }
        else
        {
          // TODO unary
        }
      }

      auto operator ()(object const& x, object const& y) const -> decltype(auto)
      {
        return data.at(type_index<N>(x.type(), y.type()))(x, y);
      }
    };

    template <typename F>
    auto apply(object const& x, object const& y) -> object
    {
      static const auto apply = apply_t<F, 2>();
      return apply(x, y);
    }
  } // inline namespace arithmetic

  template <typename T>
  auto inexact_cast(T&& x) -> decltype(auto)
  {
    if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)
    {
      return std::complex<double>(std::forward<decltype(x)>(x));
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

  auto exact(object const&) -> object;

  auto inexact(object const&) -> object;

  auto is_complex(object const&) -> bool;

  auto is_real(object const&) -> bool;

  auto is_rational(object const&) -> bool;

  auto is_integer(object const&) -> bool;

  auto is_finite(object const&) -> bool;

  auto is_infinite(object const&) -> bool;

  auto is_nan(object const&) -> bool;

  auto sqrt(object const&) -> object;

  auto pow(object const&, object const&) -> object;

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

  auto number_to_string(object const&, int) -> object;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
