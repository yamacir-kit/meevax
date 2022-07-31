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
  inline auto make_number = [](auto&& z)
  {
    if constexpr (std::is_same<typename std::decay<decltype(z)>::type, ratio>::value)
    {
      return z.simple();
    }
    else
    {
      return make(std::forward<decltype(z)>(z));
    }
  };

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

  template <typename T> auto operator * (exact_integer const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() *  b; }
  template <typename T> auto operator + (exact_integer const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() +  b; }
  template <typename T> auto operator - (exact_integer const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() -  b; }
  template <typename T> auto operator / (exact_integer const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() /  b; }
  template <typename T> auto operator % (exact_integer const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() %  b; }
  template <typename T> auto operator !=(exact_integer const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() != b; }
  template <typename T> auto operator < (exact_integer const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() <  b; }
  template <typename T> auto operator <=(exact_integer const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() <= b; }
  template <typename T> auto operator ==(exact_integer const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() == b; }
  template <typename T> auto operator > (exact_integer const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() >  b; }
  template <typename T> auto operator >=(exact_integer const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() >= b; }

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

  template <typename T> auto operator * (ratio const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() *  b; }
  template <typename T> auto operator + (ratio const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() +  b; }
  template <typename T> auto operator - (ratio const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() -  b; }
  template <typename T> auto operator / (ratio const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() /  b; }
  template <typename T> auto operator % (ratio const& a, floating_point<T> const& b)         { return a.inexact().as<double_float>() %  b; }
  template <typename T> auto operator !=(ratio const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() != b; }
  template <typename T> auto operator < (ratio const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() <  b; }
  template <typename T> auto operator <=(ratio const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() <= b; }
  template <typename T> auto operator ==(ratio const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() == b; }
  template <typename T> auto operator > (ratio const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() >  b; }
  template <typename T> auto operator >=(ratio const& a, floating_point<T> const& b) -> bool { return a.inexact().as<double_float>() >= b; }

  template <typename T> auto operator * (floating_point<T> const& a, exact_integer const& b)         { return a *  b.inexact().as<double_float>(); }
  template <typename T> auto operator + (floating_point<T> const& a, exact_integer const& b)         { return a +  b.inexact().as<double_float>(); }
  template <typename T> auto operator - (floating_point<T> const& a, exact_integer const& b)         { return a -  b.inexact().as<double_float>(); }
  template <typename T> auto operator / (floating_point<T> const& a, exact_integer const& b)         { return a /  b.inexact().as<double_float>(); }
  template <typename T> auto operator % (floating_point<T> const& a, exact_integer const& b)         { return a %  b.inexact().as<double_float>(); }
  template <typename T> auto operator !=(floating_point<T> const& a, exact_integer const& b) -> bool { return a != b.inexact().as<double_float>(); }
  template <typename T> auto operator < (floating_point<T> const& a, exact_integer const& b) -> bool { return a <  b.inexact().as<double_float>(); }
  template <typename T> auto operator <=(floating_point<T> const& a, exact_integer const& b) -> bool { return a <= b.inexact().as<double_float>(); }
  template <typename T> auto operator ==(floating_point<T> const& a, exact_integer const& b) -> bool { return a == b.inexact().as<double_float>(); }
  template <typename T> auto operator > (floating_point<T> const& a, exact_integer const& b) -> bool { return a >  b.inexact().as<double_float>(); }
  template <typename T> auto operator >=(floating_point<T> const& a, exact_integer const& b) -> bool { return a >= b.inexact().as<double_float>(); }

  template <typename T> auto operator * (floating_point<T> const& a, ratio const& b)         { return a *  b.inexact().as<double_float>(); }
  template <typename T> auto operator + (floating_point<T> const& a, ratio const& b)         { return a +  b.inexact().as<double_float>(); }
  template <typename T> auto operator - (floating_point<T> const& a, ratio const& b)         { return a -  b.inexact().as<double_float>(); }
  template <typename T> auto operator / (floating_point<T> const& a, ratio const& b)         { return a /  b.inexact().as<double_float>(); }
  template <typename T> auto operator % (floating_point<T> const& a, ratio const& b)         { return a %  b.inexact().as<double_float>(); }
  template <typename T> auto operator !=(floating_point<T> const& a, ratio const& b) -> bool { return a != b.inexact().as<double_float>(); }
  template <typename T> auto operator < (floating_point<T> const& a, ratio const& b) -> bool { return a <  b.inexact().as<double_float>(); }
  template <typename T> auto operator <=(floating_point<T> const& a, ratio const& b) -> bool { return a <= b.inexact().as<double_float>(); }
  template <typename T> auto operator ==(floating_point<T> const& a, ratio const& b) -> bool { return a == b.inexact().as<double_float>(); }
  template <typename T> auto operator > (floating_point<T> const& a, ratio const& b) -> bool { return a >  b.inexact().as<double_float>(); }
  template <typename T> auto operator >=(floating_point<T> const& a, ratio const& b) -> bool { return a >= b.inexact().as<double_float>(); }

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

  namespace inexact
  {
    template <typename Operator, typename T>
    struct application
    {
      static inline constexpr Operator operate {};

      template <typename U>
      auto inexact(U&& x)
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

      auto operator ()(const_reference x) -> value_type
      {
        return make(floating_point(operate(inexact(x.as<T>()))));
      }
    };

    template <typename F>
    auto apply(const_reference x) -> value_type
    {
      static std::unordered_map<type_index<1>, std::function<value_type (const_reference)>> apply
      {
        { type_index<1>(typeid(exact_integer)), application<F, exact_integer>() },
        // { type_index<1>(typeid(ratio        )), application<F, ratio        >() },
        { type_index<1>(typeid(single_float )), application<F, single_float >() },
        { type_index<1>(typeid(double_float )), application<F, double_float >() },
      };

      return apply.at(type_index<1>(x.type()))(x);
    }
  }

  namespace experimental
  {
    template <typename Operator, typename T, typename U>
    struct binary_operation
    {
      static inline constexpr Operator operate {};

      auto operator ()(const_reference x, const_reference y) -> value_type
      {
        return make_number(operate(x.as<T>(), y.as<U>()));
      }
    };

    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> add;
    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> sub;
    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> mul;
    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> div;
    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> mod;

    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> equal_to;
    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> not_equal_to;
    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> less;
    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> less_equal;
    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> greater;
    extern std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> greater_equal;
  }

  auto operator +(const_reference, const_reference) -> value_type;
  auto operator -(const_reference, const_reference) -> value_type;
  auto operator *(const_reference, const_reference) -> value_type;
  auto operator /(const_reference, const_reference) -> value_type;
  auto operator %(const_reference, const_reference) -> value_type;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
