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
  auto make_number = [](auto&& z)
  {
    if constexpr (std::is_same<typename std::decay<decltype(z)>::type, ratio>::value)
    {
      return z.simplify();
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
   *    auto operator <(Number const& lhs, pair::const_reference rhs)
   *    {
   *      return apply<bool>(std::less<void>(), lhs, rhs);
   *    }
   *
   * ------------------------------------------------------------------------ */
  template <typename R, typename F, typename T>
  auto apply(F&& procedure, T const& a, pair::const_reference b) -> decltype(auto)
  {
    static std::unordered_map<
      std::type_index, std::function<R (T const&, pair::const_reference)>> const overloads
    {
      { typeid(f32),           [&](T const& a, pair::const_reference b) { return procedure(a, b.as<f32          >()); } },
      { typeid(f64),           [&](T const& a, pair::const_reference b) { return procedure(a, b.as<f64          >()); } },
      { typeid(ratio),         [&](T const& a, pair::const_reference b) { return procedure(a, b.as<ratio        >()); } },
      { typeid(exact_integer), [&](T const& a, pair::const_reference b) { return procedure(a, b.as<exact_integer>()); } },
    };

    if (auto const iter = overloads.find(b.type()); iter != std::end(overloads))
    {
      return std::get<1>(*iter)(a, b);
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
   *    let operator +(Number const& lhs, pair::const_reference rhs)
   *    {
   *      return apply(add, lhs, rhs);
   *    }
   *
   * ------------------------------------------------------------------------ */
  template <typename F, typename T>
  auto apply(F&& procedure, T const& a, pair::const_reference b) -> decltype(auto)
  {
    static std::unordered_map<
      std::type_index, std::function<let (T const&, pair::const_reference)>> const overloads
    {
      { typeid(f32),           [&](T const& a, pair::const_reference b) { return make_number(procedure(a, b.as<f32          >())); } },
      { typeid(f64),           [&](T const& a, pair::const_reference b) { return make_number(procedure(a, b.as<f64          >())); } },
      { typeid(ratio),         [&](T const& a, pair::const_reference b) { return make_number(procedure(a, b.as<ratio        >())); } },
      { typeid(exact_integer), [&](T const& a, pair::const_reference b) { return make_number(procedure(a, b.as<exact_integer>())); } },
    };

    if (auto const iter = overloads.find(b.type()); iter != std::end(overloads))
    {
      return std::get<1>(*iter)(a, b);
    }
    else
    {
      throw error(make<string>(string_append("no viable operation ", demangle(typeid(F)), " with ", a, " and ", b)));
    }
  }

  /* ---- C Mathematical Functions Adapter -------------------------------------
   *
   *  Apply the given unary function to a dynamic numeric type. It is assumed
   *  that the function is given a C math function, and the numeric type is
   *  automatically converted to an inaccurate numeric type. An inaccurate
   *  numeric type is a type that corresponds to a C++ literal "0.0" and is
   *  either a float or a double. If the object does not bind a numeric type, an
   *  exception will be thrown.
   *
   *  Usage:
   *
   *    apply(std::sin, make<f64>(1.0));
   *
   *  TODO: RENAME TO apply_unary / apply_binary
   *
   * ------------------------------------------------------------------------ */
  template <typename F>
  auto apply_1(F&& cmath, pair::const_reference x) -> decltype(auto)
  {
    auto aux1 = [&](auto&& x)
    {
      return make<f64>(cmath(x.inexact().template as<f64>()));
    };

    auto aux2 = [&](auto&& x)
    {
      if (const auto y = f64(cmath(x.inexact().template as<f64>())); y.is_integer())
      {
        return make<exact_integer>(y.value);
      }
      else
      {
        return make(y);
      }
    };

    static std::unordered_map<std::type_index, procedure::applicable> const overloads
    {
      { typeid(f32),           [&](pair::const_reference x) { return aux1(x.as<f32          >()); } },
      { typeid(f64),           [&](pair::const_reference x) { return aux1(x.as<f64          >()); } },
      { typeid(ratio),         [&](pair::const_reference x) { return aux2(x.as<ratio        >()); } },
      { typeid(exact_integer), [&](pair::const_reference x) { return aux2(x.as<exact_integer>()); } },
    };

    if (auto const iter = overloads.find(x.type()); iter != std::end(overloads))
    {
      return std::get<1>(*iter)(x);
    }
    else
    {
      throw error(make<string>(string_append("no viable operation ", demangle(typeid(F)), " with ", x)));
    }
  }

  template <typename F>
  auto apply_2(F&& cmath, pair::const_reference a, pair::const_reference b)
  {
    auto inexact = [](pair::const_reference x)
    {
      static std::unordered_map<
        std::type_index, std::function<f64::value_type(pair::const_reference)>> const overloads
      {
        { typeid(f32),           [](pair::const_reference x) { return x.as<f32>()          .inexact().template as<f64>().value; } },
        { typeid(f64),           [](pair::const_reference x) { return x.as<f64>()          .inexact().template as<f64>().value; } },
        { typeid(ratio),         [](pair::const_reference x) { return x.as<ratio>()        .inexact().template as<f64>().value; } },
        { typeid(exact_integer), [](pair::const_reference x) { return x.as<exact_integer>().inexact().template as<f64>().value; } },
      };

      if (auto const iter = overloads.find(x.type()); iter != std::end(overloads))
      {
        return std::get<1>(*iter)(x);
      }
      else
      {
        return 0.0;
      }
    };

    auto aux1 = [&](auto&& x, auto&& y)
    {
      return make(floating_point(cmath(inexact(std::forward<decltype(x)>(x)),
                                       inexact(std::forward<decltype(y)>(y)))));
    };

    auto aux2 = [&](auto&& x, auto&& y)
    {
      if (floating_point const z {
            cmath(inexact(std::forward<decltype(x)>(x)),
                  inexact(std::forward<decltype(y)>(y))) }; z.is_integer())
      {
        return make<exact_integer>(z.value);
      }
      else
      {
        return make(z);
      }
    };

    if (a.is<f32>() or a.is<f64>() or b.is<f32>() or b.is<f64>())
    {
      return aux1(a, b);
    }
    else
    {
      return aux2(a, b);
    }
  }

  auto operator * (exact_integer const&, pair::const_reference) -> pair::value_type;
  auto operator + (exact_integer const&, pair::const_reference) -> pair::value_type;
  auto operator - (exact_integer const&, pair::const_reference) -> pair::value_type;
  auto operator / (exact_integer const&, pair::const_reference) -> pair::value_type;
  auto operator % (exact_integer const&, pair::const_reference) -> pair::value_type;
  auto operator ==(exact_integer const&, pair::const_reference) -> bool;
  auto operator !=(exact_integer const&, pair::const_reference) -> bool;
  auto operator < (exact_integer const&, pair::const_reference) -> bool;
  auto operator <=(exact_integer const&, pair::const_reference) -> bool;
  auto operator > (exact_integer const&, pair::const_reference) -> bool;
  auto operator >=(exact_integer const&, pair::const_reference) -> bool;

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

  template <typename T> auto operator * (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() *  b; }
  template <typename T> auto operator + (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() +  b; }
  template <typename T> auto operator - (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() -  b; }
  template <typename T> auto operator / (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() /  b; }
  template <typename T> auto operator % (exact_integer const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() %  b; }
  template <typename T> auto operator !=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() != b; }
  template <typename T> auto operator < (exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() <  b; }
  template <typename T> auto operator <=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() <= b; }
  template <typename T> auto operator ==(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() == b; }
  template <typename T> auto operator > (exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() >  b; }
  template <typename T> auto operator >=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() >= b; }

  auto operator * (ratio const&, pair::const_reference) -> pair::value_type;
  auto operator + (ratio const&, pair::const_reference) -> pair::value_type;
  auto operator - (ratio const&, pair::const_reference) -> pair::value_type;
  auto operator / (ratio const&, pair::const_reference) -> pair::value_type;
  auto operator % (ratio const&, pair::const_reference) -> pair::value_type;
  auto operator !=(ratio const&, pair::const_reference) -> boolean;
  auto operator < (ratio const&, pair::const_reference) -> boolean;
  auto operator <=(ratio const&, pair::const_reference) -> boolean;
  auto operator ==(ratio const&, pair::const_reference) -> boolean;
  auto operator > (ratio const&, pair::const_reference) -> boolean;
  auto operator >=(ratio const&, pair::const_reference) -> boolean;

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

  template <typename T> auto operator * (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() *  b; }
  template <typename T> auto operator + (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() +  b; }
  template <typename T> auto operator - (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() -  b; }
  template <typename T> auto operator / (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() /  b; }
  template <typename T> auto operator % (ratio const& a, floating_point<T> const& b)            { return a.inexact().as<f64>() %  b; }
  template <typename T> auto operator !=(ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() != b; }
  template <typename T> auto operator < (ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() <  b; }
  template <typename T> auto operator <=(ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() <= b; }
  template <typename T> auto operator ==(ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() == b; }
  template <typename T> auto operator > (ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() >  b; }
  template <typename T> auto operator >=(ratio const& a, floating_point<T> const& b) -> boolean { return a.inexact().as<f64>() >= b; }

  template <typename T> auto operator * (floating_point<T> const& a, pair::const_reference b) { return apply(mul, a, b); }
  template <typename T> auto operator + (floating_point<T> const& a, pair::const_reference b) { return apply(add, a, b); }
  template <typename T> auto operator - (floating_point<T> const& a, pair::const_reference b) { return apply(sub, a, b); }
  template <typename T> auto operator / (floating_point<T> const& a, pair::const_reference b) { return apply(div, a, b); }
  template <typename T> auto operator % (floating_point<T> const& a, pair::const_reference b) { return apply(mod, a, b); }
  template <typename T> auto operator !=(floating_point<T> const& a, pair::const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a != b; }, a, b); }
  template <typename T> auto operator < (floating_point<T> const& a, pair::const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a <  b; }, a, b); }
  template <typename T> auto operator <=(floating_point<T> const& a, pair::const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a <= b; }, a, b); }
  template <typename T> auto operator ==(floating_point<T> const& a, pair::const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a == b; }, a, b); }
  template <typename T> auto operator > (floating_point<T> const& a, pair::const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a >  b; }, a, b); }
  template <typename T> auto operator >=(floating_point<T> const& a, pair::const_reference b) { return apply<boolean>([](auto&& a, auto&& b) { return a >= b; }, a, b); }

  template <typename T> auto operator * (floating_point<T> const& a, exact_integer const& b)            { return a *  b.inexact().as<f64>(); }
  template <typename T> auto operator + (floating_point<T> const& a, exact_integer const& b)            { return a +  b.inexact().as<f64>(); }
  template <typename T> auto operator - (floating_point<T> const& a, exact_integer const& b)            { return a -  b.inexact().as<f64>(); }
  template <typename T> auto operator / (floating_point<T> const& a, exact_integer const& b)            { return a /  b.inexact().as<f64>(); }
  template <typename T> auto operator % (floating_point<T> const& a, exact_integer const& b)            { return a %  b.inexact().as<f64>(); }
  template <typename T> auto operator !=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a != b.inexact().as<f64>(); }
  template <typename T> auto operator < (floating_point<T> const& a, exact_integer const& b) -> boolean { return a <  b.inexact().as<f64>(); }
  template <typename T> auto operator <=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a <= b.inexact().as<f64>(); }
  template <typename T> auto operator ==(floating_point<T> const& a, exact_integer const& b) -> boolean { return a == b.inexact().as<f64>(); }
  template <typename T> auto operator > (floating_point<T> const& a, exact_integer const& b) -> boolean { return a >  b.inexact().as<f64>(); }
  template <typename T> auto operator >=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a >= b.inexact().as<f64>(); }

  template <typename T> auto operator * (floating_point<T> const& a, ratio const& b)            { return a *  b.inexact().as<f64>(); }
  template <typename T> auto operator + (floating_point<T> const& a, ratio const& b)            { return a +  b.inexact().as<f64>(); }
  template <typename T> auto operator - (floating_point<T> const& a, ratio const& b)            { return a -  b.inexact().as<f64>(); }
  template <typename T> auto operator / (floating_point<T> const& a, ratio const& b)            { return a /  b.inexact().as<f64>(); }
  template <typename T> auto operator % (floating_point<T> const& a, ratio const& b)            { return a %  b.inexact().as<f64>(); }
  template <typename T> auto operator !=(floating_point<T> const& a, ratio const& b) -> boolean { return a != b.inexact().as<f64>(); }
  template <typename T> auto operator < (floating_point<T> const& a, ratio const& b) -> boolean { return a <  b.inexact().as<f64>(); }
  template <typename T> auto operator <=(floating_point<T> const& a, ratio const& b) -> boolean { return a <= b.inexact().as<f64>(); }
  template <typename T> auto operator ==(floating_point<T> const& a, ratio const& b) -> boolean { return a == b.inexact().as<f64>(); }
  template <typename T> auto operator > (floating_point<T> const& a, ratio const& b) -> boolean { return a >  b.inexact().as<f64>(); }
  template <typename T> auto operator >=(floating_point<T> const& a, ratio const& b) -> boolean { return a >= b.inexact().as<f64>(); }

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

  template <typename Result>
  auto resolve(std::unordered_map<std::type_index, std::function<Result(pair::const_reference)>> const& overloads, pair::const_reference x)
  {
    if (auto const iter = overloads.find(x.type()); iter != std::end(overloads))
    {
      return std::get<1>(*iter)(x);
    }
    else
    {
      return Result(); // NOTE N4296 Section 8.5 (6.1)
    }
  }

  auto exact = [](pair::const_reference z)
  {
    static std::unordered_map<std::type_index, procedure::applicable> const overloads
    {
      { typeid(f32),           [](pair::const_reference x) { return make_number(x.as<f32          >().as_exact()); } },
      { typeid(f64),           [](pair::const_reference x) { return make_number(x.as<f64          >().as_exact()); } },
      { typeid(ratio),         [](pair::const_reference x) { return make_number(x.as<ratio        >().as_exact()); } },
      { typeid(exact_integer), [](pair::const_reference x) { return make_number(x.as<exact_integer>().as_exact()); } },
    };

    return resolve(overloads, z);
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
