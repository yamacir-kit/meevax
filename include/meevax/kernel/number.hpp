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

#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <typeindex>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/floating_point.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/ratio.hpp>

namespace meevax
{
inline namespace kernel
{
  auto is_rational = [](let const& x)
  {
    return x.is<ratio>() or (x.is_also<number>() and x.as<number>().is_integer());
  };

  auto is_real = [](let const& x)
  {
    return x.is<single_float>() or x.is<double_float>() or is_rational(x);
  };

  auto make_number = [](auto&& z)
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

  /* ---- Binary Numerical Comparator Overloaings Adapter ----------------------
   *
   *  Dispatch static numeric vs. dynamic numerical operations to static
   *  overloads. If the rvalue type binds a non-numeric type, an exception is
   *  thrown.
   *
   *  Usage:
   *
   *    auto operator <(Number const& lhs, const_reference rhs)
   *    {
   *      return apply<bool>(std::less<void>(), lhs, rhs);
   *    }
   *
   * ------------------------------------------------------------------------ */
  template <typename R, typename F, typename T>
  auto apply(F&& procedure, T const& a, const_reference b) -> decltype(auto)
  {
    static std::unordered_map<
      std::type_index, std::function<R (T const&, const_reference)>> const overloads
    {
      { typeid(single_float),  [&](T const& a, const_reference b) { return procedure(a, b.as<single_float >()); } },
      { typeid(double_float),  [&](T const& a, const_reference b) { return procedure(a, b.as<double_float >()); } },
      { typeid(ratio),         [&](T const& a, const_reference b) { return procedure(a, b.as<ratio        >()); } },
      { typeid(exact_integer), [&](T const& a, const_reference b) { return procedure(a, b.as<exact_integer>()); } },
    };

    if (auto const iter = overloads.find(b.type()); iter != std::end(overloads))
    {
      return cdr(*iter)(a, b);
    }
    else
    {
      throw error(make<string>(string_append("no viable operation ", demangle(typeid(F)), " with ", a, " and ", b)));
    }
  }

  /* ---- Binary Numerical Operator Overloaings Adapter ------------------------
   *
   *  Dispatch static numeric vs. dynamic numerical operations to static
   *  overloads. If the rvalue type binds a non-numeric type, an exception is
   *  thrown.
   *
   *  Usage:
   *
   *    let operator +(Number const& lhs, const_reference rhs)
   *    {
   *      return apply(add, lhs, rhs);
   *    }
   *
   * ------------------------------------------------------------------------ */
  template <typename F, typename T>
  auto apply(F&& procedure, T const& a, const_reference b) -> decltype(auto)
  {
    static std::unordered_map<
      std::type_index, std::function<let (T const&, const_reference)>> const overloads
    {
      { typeid(single_float),  [&](T const& a, const_reference b) { return make_number(procedure(a, b.as<single_float >())); } },
      { typeid(double_float),  [&](T const& a, const_reference b) { return make_number(procedure(a, b.as<double_float >())); } },
      { typeid(ratio),         [&](T const& a, const_reference b) { return make_number(procedure(a, b.as<ratio        >())); } },
      { typeid(exact_integer), [&](T const& a, const_reference b) { return make_number(procedure(a, b.as<exact_integer>())); } },
    };

    if (auto const iter = overloads.find(b.type()); iter != std::end(overloads))
    {
      return cdr(*iter)(a, b);
    }
    else
    {
      throw error(make<string>(string_append("no viable operation ", demangle(typeid(F)), " with ", a, " and ", b)));
    }
  }

  auto operator * (exact_integer const&, const_reference) -> object;
  auto operator + (exact_integer const&, const_reference) -> object;
  auto operator - (exact_integer const&, const_reference) -> object;
  auto operator / (exact_integer const&, const_reference) -> object;
  auto operator % (exact_integer const&, const_reference) -> object;
  auto operator ==(exact_integer const&, const_reference) -> bool;
  auto operator !=(exact_integer const&, const_reference) -> bool;
  auto operator < (exact_integer const&, const_reference) -> bool;
  auto operator <=(exact_integer const&, const_reference) -> bool;
  auto operator > (exact_integer const&, const_reference) -> bool;
  auto operator >=(exact_integer const&, const_reference) -> bool;

  auto operator * (exact_integer const&, exact_integer const&) -> exact_integer;
  auto operator + (exact_integer const&, exact_integer const&) -> exact_integer;
  auto operator - (exact_integer const&, exact_integer const&) -> exact_integer;
  auto operator / (exact_integer const&, exact_integer const&) -> ratio;
  auto operator % (exact_integer const&, exact_integer const&) -> exact_integer;
  auto operator !=(exact_integer const&, exact_integer const&) -> boolean;
  auto operator < (exact_integer const&, exact_integer const&) -> boolean;
  auto operator <=(exact_integer const&, exact_integer const&) -> boolean;
  auto operator ==(exact_integer const&, exact_integer const&) -> boolean;
  auto operator > (exact_integer const&, exact_integer const&) -> boolean;
  auto operator >=(exact_integer const&, exact_integer const&) -> boolean;

  auto operator * (exact_integer const&, ratio const&) -> ratio;
  auto operator + (exact_integer const&, ratio const&) -> ratio;
  auto operator - (exact_integer const&, ratio const&) -> ratio;
  auto operator / (exact_integer const&, ratio const&) -> ratio;
  auto operator % (exact_integer const&, ratio const&) -> ratio;
  auto operator !=(exact_integer const&, ratio const&) -> boolean;
  auto operator < (exact_integer const&, ratio const&) -> boolean;
  auto operator <=(exact_integer const&, ratio const&) -> boolean;
  auto operator ==(exact_integer const&, ratio const&) -> boolean;
  auto operator > (exact_integer const&, ratio const&) -> boolean;
  auto operator >=(exact_integer const&, ratio const&) -> boolean;

  template <typename T> auto operator * (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() *  b; }
  template <typename T> auto operator + (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() +  b; }
  template <typename T> auto operator - (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() -  b; }
  template <typename T> auto operator / (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() /  b; }
  template <typename T> auto operator % (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() %  b; }
  template <typename T> auto operator !=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() != b; }
  template <typename T> auto operator < (exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() <  b; }
  template <typename T> auto operator <=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() <= b; }
  template <typename T> auto operator ==(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() == b; }
  template <typename T> auto operator > (exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() >  b; }
  template <typename T> auto operator >=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() >= b; }

  auto operator * (ratio const&, const_reference) -> object;
  auto operator + (ratio const&, const_reference) -> object;
  auto operator - (ratio const&, const_reference) -> object;
  auto operator / (ratio const&, const_reference) -> object;
  auto operator % (ratio const&, const_reference) -> object;
  auto operator !=(ratio const&, const_reference) -> boolean;
  auto operator < (ratio const&, const_reference) -> boolean;
  auto operator <=(ratio const&, const_reference) -> boolean;
  auto operator ==(ratio const&, const_reference) -> boolean;
  auto operator > (ratio const&, const_reference) -> boolean;
  auto operator >=(ratio const&, const_reference) -> boolean;

  auto operator * (ratio const&, exact_integer const&) -> ratio;
  auto operator + (ratio const&, exact_integer const&) -> ratio;
  auto operator - (ratio const&, exact_integer const&) -> ratio;
  auto operator / (ratio const&, exact_integer const&) -> ratio;
  auto operator % (ratio const&, exact_integer const&) -> ratio;
  auto operator !=(ratio const&, exact_integer const&) -> boolean;
  auto operator < (ratio const&, exact_integer const&) -> boolean;
  auto operator <=(ratio const&, exact_integer const&) -> boolean;
  auto operator ==(ratio const&, exact_integer const&) -> boolean;
  auto operator > (ratio const&, exact_integer const&) -> boolean;
  auto operator >=(ratio const&, exact_integer const&) -> boolean;

  auto operator * (ratio const&, ratio const&) -> ratio;
  auto operator + (ratio const&, ratio const&) -> ratio;
  auto operator - (ratio const&, ratio const&) -> ratio;
  auto operator / (ratio const&, ratio const&) -> ratio;
  auto operator % (ratio const&, ratio const&) -> ratio;
  auto operator ==(ratio const&, ratio const&) -> boolean;
  auto operator !=(ratio const&, ratio const&) -> boolean;
  auto operator < (ratio const&, ratio const&) -> boolean;
  auto operator <=(ratio const&, ratio const&) -> boolean;
  auto operator > (ratio const&, ratio const&) -> boolean;
  auto operator >=(ratio const&, ratio const&) -> boolean;

  template <typename T> auto operator * (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() *  b; }
  template <typename T> auto operator + (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() +  b; }
  template <typename T> auto operator - (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() -  b; }
  template <typename T> auto operator / (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() /  b; }
  template <typename T> auto operator % (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<double_float>() %  b; }
  template <typename T> auto operator !=(ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() != b; }
  template <typename T> auto operator < (ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() <  b; }
  template <typename T> auto operator <=(ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() <= b; }
  template <typename T> auto operator ==(ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() == b; }
  template <typename T> auto operator > (ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() >  b; }
  template <typename T> auto operator >=(ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<double_float>() >= b; }

  template <typename T> auto operator * (floating_point<T> const& a, const_reference b) { return apply(mul, a, b); }
  template <typename T> auto operator + (floating_point<T> const& a, const_reference b) { return apply(add, a, b); }
  template <typename T> auto operator - (floating_point<T> const& a, const_reference b) { return apply(sub, a, b); }
  template <typename T> auto operator / (floating_point<T> const& a, const_reference b) { return apply(div, a, b); }
  template <typename T> auto operator % (floating_point<T> const& a, const_reference b) { return apply(mod, a, b); }
  template <typename T> auto operator !=(floating_point<T> const& a, const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a != b; }, a, b); }
  template <typename T> auto operator < (floating_point<T> const& a, const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a <  b; }, a, b); }
  template <typename T> auto operator <=(floating_point<T> const& a, const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a <= b; }, a, b); }
  template <typename T> auto operator ==(floating_point<T> const& a, const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a == b; }, a, b); }
  template <typename T> auto operator > (floating_point<T> const& a, const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a >  b; }, a, b); }
  template <typename T> auto operator >=(floating_point<T> const& a, const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a >= b; }, a, b); }

  template <typename T> auto operator * (floating_point<T> const& a, exact_integer const& b)            { return a *  b.inexact().as<double_float>(); }
  template <typename T> auto operator + (floating_point<T> const& a, exact_integer const& b)            { return a +  b.inexact().as<double_float>(); }
  template <typename T> auto operator - (floating_point<T> const& a, exact_integer const& b)            { return a -  b.inexact().as<double_float>(); }
  template <typename T> auto operator / (floating_point<T> const& a, exact_integer const& b)            { return a /  b.inexact().as<double_float>(); }
  template <typename T> auto operator % (floating_point<T> const& a, exact_integer const& b)            { return a %  b.inexact().as<double_float>(); }
  template <typename T> auto operator !=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a != b.inexact().as<double_float>(); }
  template <typename T> auto operator < (floating_point<T> const& a, exact_integer const& b) -> boolean { return a <  b.inexact().as<double_float>(); }
  template <typename T> auto operator <=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a <= b.inexact().as<double_float>(); }
  template <typename T> auto operator ==(floating_point<T> const& a, exact_integer const& b) -> boolean { return a == b.inexact().as<double_float>(); }
  template <typename T> auto operator > (floating_point<T> const& a, exact_integer const& b) -> boolean { return a >  b.inexact().as<double_float>(); }
  template <typename T> auto operator >=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a >= b.inexact().as<double_float>(); }

  template <typename T> auto operator * (floating_point<T> const& a, ratio const& b)            { return a *  b.inexact().as<double_float>(); }
  template <typename T> auto operator + (floating_point<T> const& a, ratio const& b)            { return a +  b.inexact().as<double_float>(); }
  template <typename T> auto operator - (floating_point<T> const& a, ratio const& b)            { return a -  b.inexact().as<double_float>(); }
  template <typename T> auto operator / (floating_point<T> const& a, ratio const& b)            { return a /  b.inexact().as<double_float>(); }
  template <typename T> auto operator % (floating_point<T> const& a, ratio const& b)            { return a %  b.inexact().as<double_float>(); }
  template <typename T> auto operator !=(floating_point<T> const& a, ratio const& b) -> boolean { return a != b.inexact().as<double_float>(); }
  template <typename T> auto operator < (floating_point<T> const& a, ratio const& b) -> boolean { return a <  b.inexact().as<double_float>(); }
  template <typename T> auto operator <=(floating_point<T> const& a, ratio const& b) -> boolean { return a <= b.inexact().as<double_float>(); }
  template <typename T> auto operator ==(floating_point<T> const& a, ratio const& b) -> boolean { return a == b.inexact().as<double_float>(); }
  template <typename T> auto operator > (floating_point<T> const& a, ratio const& b) -> boolean { return a >  b.inexact().as<double_float>(); }
  template <typename T> auto operator >=(floating_point<T> const& a, ratio const& b) -> boolean { return a >= b.inexact().as<double_float>(); }

  template <typename T, typename U> auto operator * (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value * b.value); }
  template <typename T, typename U> auto operator + (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value + b.value); }
  template <typename T, typename U> auto operator - (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value - b.value); }
  template <typename T, typename U> auto operator / (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value / b.value); }
  template <typename T, typename U> auto operator % (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(std::remainder(a.value, b.value)); }
  template <typename T, typename U> auto operator ==(floating_point<T> const& a, floating_point<U> const& b) -> boolean
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
  template <typename T, typename U> auto operator !=(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return not (a == b); }
  template <typename T, typename U> auto operator < (floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value <  b.value; }
  template <typename T, typename U> auto operator <=(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value <= b.value; }
  template <typename T, typename U> auto operator > (floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value >  b.value; }
  template <typename T, typename U> auto operator >=(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value >= b.value; }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
