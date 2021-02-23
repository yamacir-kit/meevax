#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <typeindex>

#include <boost/math/constants/constants.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/floating_point.hpp>
#include <meevax/kernel/ratio.hpp>

namespace meevax
{
inline namespace kernel
{
  auto make_number = [](auto&& z)
  {
    if constexpr (std::is_same<typename std::decay<decltype(z)>::type, ratio>::value)
    {
      if (auto const x = z.reduce(); x.is_integer())
      {
        return car(x);
      }
      else
      {
        return make(x);
      }
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
   *    auto operator <(Number const& lhs, object const& rhs)
   *    {
   *      return apply<bool>(std::less<void>(), lhs, rhs);
   *    }
   *
   * ------------------------------------------------------------------------ */
  template <typename R, typename F, typename T>
  auto apply(F&& procedure, T const& a, object const& b) -> decltype(auto)
  {
    static std::unordered_map<
      std::type_index, std::function<R (T const&, object const&)>> const overloads
    {
      { typeid(single_float),  [&](T const& a, let const& b) { return procedure(a, b.as<single_float> ()); } },
      { typeid(double_float),  [&](T const& a, let const& b) { return procedure(a, b.as<double_float> ()); } },
      { typeid(ratio),         [&](T const& a, let const& b) { return procedure(a, b.as<ratio>        ()); } },
      { typeid(exact_integer), [&](T const& a, let const& b) { return procedure(a, b.as<exact_integer>()); } },
    };

    if (auto const iter { overloads.find(b.type()) }; iter != std::end(overloads))
    {
      return std::get<1>(*iter)(a, b);
    }
    else
    {
      throw error("no viable operation '", typeid(F).name(), "' with ", a, " and ", b);
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
   *    let operator +(Number const& lhs, object const& rhs)
   *    {
   *      return apply(std::plus<void>(), lhs, rhs);
   *    }
   *
   * ------------------------------------------------------------------------ */
  template <typename F, typename T>
  auto apply(F&& procedure, T const& a, object const& b) -> decltype(auto)
  {
    static std::unordered_map<
      std::type_index, std::function<object (T const&, object const&)>> const overloads
    {
      { typeid(single_float),  [&](T const& a, let const& b) { return make_number(procedure(a, b.as<single_float >())); } },
      { typeid(double_float),  [&](T const& a, let const& b) { return make_number(procedure(a, b.as<double_float >())); } },
      { typeid(ratio),         [&](T const& a, let const& b) { return make_number(procedure(a, b.as<ratio        >())); } },
      { typeid(exact_integer), [&](T const& a, let const& b) { return make_number(procedure(a, b.as<exact_integer>())); } },
    };

    if (auto const iter = overloads.find(b.type()); iter != std::end(overloads))
    {
      return std::get<1>(*iter)(a, b);
    }
    else
    {
      throw error("no viable operation '", typeid(F).name(), "' with ", a, " and ", b);
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
   *    apply(std::sin, make<double_float>(1.0));
   *
   * ------------------------------------------------------------------------ */
  template <typename F>
  auto apply_1(F&& cmath, object const& x) -> decltype(auto)
  {
    auto aux1 = [&](auto&& x)
    {
      return make(floating_point(cmath(x.template as_inexact<decltype(0.0)>())));
    };

    auto aux2 = [&](auto&& x)
    {
      if (floating_point const y { cmath(x.template as_inexact<decltype(0.0)>()) }; y.is_integer())
      {
        return make<exact_integer>(y.value);
      }
      else
      {
        return make(y);
      }
    };

    static std::unordered_map<
      std::type_index, std::function<object (object const&)>> const overloads
    {
      { typeid(single_float),  [&](let const& x) { return aux1(x.as<single_float >()); } },
      { typeid(double_float),  [&](let const& x) { return aux1(x.as<double_float >()); } },
      { typeid(ratio),         [&](let const& x) { return aux2(x.as<ratio        >()); } },
      { typeid(exact_integer), [&](let const& x) { return aux2(x.as<exact_integer>()); } },
    };

    if (auto const iter = overloads.find(x.type()); iter != std::end(overloads))
    {
      return std::get<1>(*iter)(x);
    }
    else
    {
      throw error("no viable operation '", typeid(F).name(), "' with ", x);
    }
  }

  template <typename F>
  auto apply_2(F&& cmath, object const& a, object const& b)
  {
    auto inexact = [](let const& x)
    {
      static std::unordered_map<
        std::type_index, std::function<decltype(0.0) (object const&)>> const overloads
      {
        { typeid(single_float),  [](let const& x) { return x.as<single_float>() .as_inexact<decltype(0.0)>().value; } },
        { typeid(double_float),  [](let const& x) { return x.as<double_float>() .as_inexact<decltype(0.0)>().value; } },
        { typeid(ratio),         [](let const& x) { return x.as<ratio>()        .as_inexact<decltype(0.0)>().value; } },
        { typeid(exact_integer), [](let const& x) { return x.as<exact_integer>().as_inexact<decltype(0.0)>().value; } },
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

    if (a.is<single_float>() or a.is<double_float>() or
        b.is<single_float>() or b.is<double_float>())
    {
      return aux1(a, b);
    }
    else
    {
      return aux2(a, b);
    }
  }

  auto operator * (exact_integer const&, object const&) -> object;
  auto operator + (exact_integer const&, object const&) -> object;
  auto operator - (exact_integer const&, object const&) -> object;
  auto operator / (exact_integer const&, object const&) -> object;
  auto operator % (exact_integer const&, object const&) -> object;
  auto operator ==(exact_integer const&, object const&) -> bool;
  auto operator !=(exact_integer const&, object const&) -> bool;
  auto operator < (exact_integer const&, object const&) -> bool;
  auto operator <=(exact_integer const&, object const&) -> bool;
  auto operator > (exact_integer const&, object const&) -> bool;
  auto operator >=(exact_integer const&, object const&) -> bool;

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

  template <typename T> auto operator * (exact_integer const& a, floating_point<T> const& b)            { return a.as_inexact<T>() *  b; }
  template <typename T> auto operator + (exact_integer const& a, floating_point<T> const& b)            { return a.as_inexact<T>() +  b; }
  template <typename T> auto operator - (exact_integer const& a, floating_point<T> const& b)            { return a.as_inexact<T>() -  b; }
  template <typename T> auto operator / (exact_integer const& a, floating_point<T> const& b)            { return a.as_inexact<T>() /  b; }
  template <typename T> auto operator % (exact_integer const& a, floating_point<T> const& b)            { return a.as_inexact<T>() %  b; }
  template <typename T> auto operator !=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() != b; }
  template <typename T> auto operator < (exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() <  b; }
  template <typename T> auto operator <=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() <= b; }
  template <typename T> auto operator ==(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() == b; }
  template <typename T> auto operator > (exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() >  b; }
  template <typename T> auto operator >=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() >= b; }

  auto operator * (ratio const&, object const&) -> object;
  auto operator + (ratio const&, object const&) -> object;
  auto operator - (ratio const&, object const&) -> object;
  auto operator / (ratio const&, object const&) -> object;
  auto operator % (ratio const&, object const&) -> object;
  auto operator !=(ratio const&, object const&) -> boolean;
  auto operator < (ratio const&, object const&) -> boolean;
  auto operator <=(ratio const&, object const&) -> boolean;
  auto operator ==(ratio const&, object const&) -> boolean;
  auto operator > (ratio const&, object const&) -> boolean;
  auto operator >=(ratio const&, object const&) -> boolean;

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

  template <typename T> auto operator * (ratio const& a, floating_point<T> const& b)            { return a.as_inexact<T>() *  b; }
  template <typename T> auto operator + (ratio const& a, floating_point<T> const& b)            { return a.as_inexact<T>() +  b; }
  template <typename T> auto operator - (ratio const& a, floating_point<T> const& b)            { return a.as_inexact<T>() -  b; }
  template <typename T> auto operator / (ratio const& a, floating_point<T> const& b)            { return a.as_inexact<T>() /  b; }
  template <typename T> auto operator % (ratio const& a, floating_point<T> const& b)            { return a.as_inexact<T>() %  b; }
  template <typename T> auto operator !=(ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() != b; }
  template <typename T> auto operator < (ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() <  b; }
  template <typename T> auto operator <=(ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() <= b; }
  template <typename T> auto operator ==(ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() == b; }
  template <typename T> auto operator > (ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() >  b; }
  template <typename T> auto operator >=(ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() >= b; }

  template <typename T> auto operator * (floating_point<T> const& a, object const& b) { return apply         ([](auto&& a, auto&& b) { return a *  b; }, a, b); }
  template <typename T> auto operator + (floating_point<T> const& a, object const& b) { return apply         ([](auto&& a, auto&& b) { return a +  b; }, a, b); }
  template <typename T> auto operator - (floating_point<T> const& a, object const& b) { return apply         ([](auto&& a, auto&& b) { return a -  b; }, a, b); }
  template <typename T> auto operator / (floating_point<T> const& a, object const& b) { return apply         ([](auto&& a, auto&& b) { return a /  b; }, a, b); }
  template <typename T> auto operator % (floating_point<T> const& a, object const& b) { return apply         ([](auto&& a, auto&& b) { return a %  b; }, a, b); }
  template <typename T> auto operator !=(floating_point<T> const& a, object const& b) { return apply<boolean>([](auto&& a, auto&& b) { return a != b; }, a, b); }
  template <typename T> auto operator < (floating_point<T> const& a, object const& b) { return apply<boolean>([](auto&& a, auto&& b) { return a <  b; }, a, b); }
  template <typename T> auto operator <=(floating_point<T> const& a, object const& b) { return apply<boolean>([](auto&& a, auto&& b) { return a <= b; }, a, b); }
  template <typename T> auto operator ==(floating_point<T> const& a, object const& b) { return apply<boolean>([](auto&& a, auto&& b) { return a == b; }, a, b); }
  template <typename T> auto operator > (floating_point<T> const& a, object const& b) { return apply<boolean>([](auto&& a, auto&& b) { return a >  b; }, a, b); }
  template <typename T> auto operator >=(floating_point<T> const& a, object const& b) { return apply<boolean>([](auto&& a, auto&& b) { return a >= b; }, a, b); }

  template <typename T> auto operator * (floating_point<T> const& a, exact_integer const& b)            { return a *  b.as_inexact<T>(); }
  template <typename T> auto operator + (floating_point<T> const& a, exact_integer const& b)            { return a +  b.as_inexact<T>(); }
  template <typename T> auto operator - (floating_point<T> const& a, exact_integer const& b)            { return a -  b.as_inexact<T>(); }
  template <typename T> auto operator / (floating_point<T> const& a, exact_integer const& b)            { return a /  b.as_inexact<T>(); }
  template <typename T> auto operator % (floating_point<T> const& a, exact_integer const& b)            { return a %  b.as_inexact<T>(); }
  template <typename T> auto operator !=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a != b.as_inexact<T>(); }
  template <typename T> auto operator < (floating_point<T> const& a, exact_integer const& b) -> boolean { return a <  b.as_inexact<T>(); }
  template <typename T> auto operator <=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a <= b.as_inexact<T>(); }
  template <typename T> auto operator ==(floating_point<T> const& a, exact_integer const& b) -> boolean { return a == b.as_inexact<T>(); }
  template <typename T> auto operator > (floating_point<T> const& a, exact_integer const& b) -> boolean { return a >  b.as_inexact<T>(); }
  template <typename T> auto operator >=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a >= b.as_inexact<T>(); }

  template <typename T> auto operator * (floating_point<T> const& a, ratio const& b)            { return a *  b.as_inexact<T>(); }
  template <typename T> auto operator + (floating_point<T> const& a, ratio const& b)            { return a +  b.as_inexact<T>(); }
  template <typename T> auto operator - (floating_point<T> const& a, ratio const& b)            { return a -  b.as_inexact<T>(); }
  template <typename T> auto operator / (floating_point<T> const& a, ratio const& b)            { return a /  b.as_inexact<T>(); }
  template <typename T> auto operator % (floating_point<T> const& a, ratio const& b)            { return a %  b.as_inexact<T>(); }
  template <typename T> auto operator !=(floating_point<T> const& a, ratio const& b) -> boolean { return a != b.as_inexact<T>(); }
  template <typename T> auto operator < (floating_point<T> const& a, ratio const& b) -> boolean { return a <  b.as_inexact<T>(); }
  template <typename T> auto operator <=(floating_point<T> const& a, ratio const& b) -> boolean { return a <= b.as_inexact<T>(); }
  template <typename T> auto operator ==(floating_point<T> const& a, ratio const& b) -> boolean { return a == b.as_inexact<T>(); }
  template <typename T> auto operator > (floating_point<T> const& a, ratio const& b) -> boolean { return a >  b.as_inexact<T>(); }
  template <typename T> auto operator >=(floating_point<T> const& a, ratio const& b) -> boolean { return a >= b.as_inexact<T>(); }

  template <typename T, typename U> auto operator * (floating_point<T> const& a, floating_point<U> const& b)            { return floating_point(a.value * b.value); }
  template <typename T, typename U> auto operator + (floating_point<T> const& a, floating_point<U> const& b)            { return floating_point(a.value + b.value); }
  template <typename T, typename U> auto operator - (floating_point<T> const& a, floating_point<U> const& b)            { return floating_point(a.value - b.value); }
  template <typename T, typename U> auto operator / (floating_point<T> const& a, floating_point<U> const& b)            { return floating_point(a.value / b.value); }
  template <typename T, typename U> auto operator % (floating_point<T> const& a, floating_point<U> const& b)            { return floating_point(std::remainder(a.value, b.value)); }
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

  template <typename T>
  T resolve(std::unordered_map<std::type_index, std::function<T (object const&)>> const& overloads, object const& x)
  {
    if (auto const iter = overloads.find(x.type()); iter != std::end(overloads))
    {
      return std::get<1>(*iter)(x);
    }
    else
    {
      return T(); // NOTE N4296 Section 8.5 (6.1)
    }
  }

  auto exact = [](let const& z)
  {
    static std::unordered_map<
      std::type_index, std::function<object (object const&)>> const overloads
    {
      { typeid(single_float),  [](let const& x) { return make_number(x.as<single_float >().as_exact()); } },
      { typeid(double_float),  [](let const& x) { return make_number(x.as<double_float >().as_exact()); } },
      { typeid(ratio),         [](let const& x) { return make_number(x.as<ratio        >().as_exact()); } },
      { typeid(exact_integer), [](let const& x) { return make_number(x.as<exact_integer>().as_exact()); } },
    };

    return resolve(overloads, z);
  };

  auto inexact = [](let const& z)
  {
    static std::unordered_map<
      std::type_index, std::function<object (object const&)>> const overloads
    {
      { typeid(single_float),  [](let const& x) { return make(x.as<single_float >().as_inexact<decltype(0.0)>()); } },
      { typeid(double_float),  [](let const& x) { return make(x.as<double_float >().as_inexact<decltype(0.0)>()); } },
      { typeid(ratio),         [](let const& x) { return make(x.as<ratio        >().as_inexact<decltype(0.0)>()); } },
      { typeid(exact_integer), [](let const& x) { return make(x.as<exact_integer>().as_inexact<decltype(0.0)>()); } },
    };

    return resolve(overloads, z);
  };

  auto is_nan = [](object const& x)
  {
    static std::unordered_map<
      std::type_index, std::function<bool (object const&)>> const overloads
    {
      { typeid(single_float), [](let const& x) { return std::isnan(x.as<single_float>()); } },
      { typeid(double_float), [](let const& x) { return std::isnan(x.as<double_float>()); } },
    };

    return resolve(overloads, x);
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
