#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <typeindex>

#include <boost/math/constants/constants.hpp>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/floating_point.hpp>
#include <meevax/kernel/ratio.hpp>

namespace meevax
{
inline namespace kernel
{
  // TODO RENAME TO make_number (current make_number => read_number)
  auto make_reduce = [](auto&& z)
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
    // else if constexpr (std::is_same<typename std::decay<decltype(z)>::type, floating_point<double>>::value)
    // {
    //   if (z.is_integer())
    //   {
    //     return make<exact_integer>(z.value);
    //   }
    //   else
    //   {
    //     return make(z);
    //   }
    // }
    else
    {
      return make(std::forward<decltype(z)>(z));
    }
  };

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

  template <typename F, typename T>
  auto apply(F&& procedure, T const& a, object const& b) -> decltype(auto)
  {
    static std::unordered_map<
      std::type_index, std::function<object (T const&, object const&)>> const overloads
    {
      { typeid(single_float),  [&](T const& a, let const& b) { return make_reduce(procedure(a, b.as<single_float> ())); } },
      { typeid(double_float),  [&](T const& a, let const& b) { return make_reduce(procedure(a, b.as<double_float> ())); } },
      { typeid(ratio),         [&](T const& a, let const& b) { return make_reduce(procedure(a, b.as<ratio>        ())); } },
      { typeid(exact_integer), [&](T const& a, let const& b) { return make_reduce(procedure(a, b.as<exact_integer>())); } },
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

  // template <typename F>
  // auto apply(F&& f, object const& x) -> decltype(auto)
  // {
  //   static std::unordered_map<
  //     std::type_index, std::function<object (object const&)>> const overloads
  //   {
  //     { typeid(single_float),  [&](let const& x) { return make_reduce(f(x.as<single_float>() .as_inexact<decltype(0.0)>())); } },
  //     { typeid(double_float),  [&](let const& x) { return make_reduce(f(x.as<double_float>() .as_inexact<decltype(0.0)>())); } },
  //     { typeid(ratio),         [&](let const& x) { return make_reduce(f(x.as<ratio>()        .as_inexact<decltype(0.0)>())); } },
  //     { typeid(exact_integer), [&](let const& x) { return make_reduce(f(x.as<exact_integer>().as_inexact<decltype(0.0)>())); } },
  //   };
  //
  //   if (auto const iter = overloads.find(x.type()); iter != std::end(overloads))
  //   {
  //     return std::get<1>(*iter)(x);
  //   }
  //   else
  //   {
  //     throw error("no viable operation '", typeid(F).name(), "' with ", x);
  //   }
  // }

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
  auto operator / (exact_integer const&, exact_integer const&) -> exact_integer;
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
  template <typename T, typename U> auto operator % (floating_point<T> const& a, floating_point<U> const& b)            { return floating_point(std::fmod(a.value, b.value)); }
  template <typename T, typename U> auto operator !=(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value != b.value; } // TODO EPSILON
  template <typename T, typename U> auto operator < (floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value <  b.value; } // TODO EPSILON
  template <typename T, typename U> auto operator <=(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value <= b.value; } // TODO EPSILON
  template <typename T, typename U> auto operator ==(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value == b.value; } // TODO EPSILON
  template <typename T, typename U> auto operator > (floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value >  b.value; } // TODO EPSILON
  template <typename T, typename U> auto operator >=(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value >= b.value; } // TODO EPSILON

  template <typename T>
  T resolve(std::unordered_map<std::type_index, std::function<T (object const&)>> const& overloads, object const& x)
  {
    if (auto const iter { overloads.find(x.type()) }; iter != std::end(overloads))
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
      { typeid(single_float),  [](let const& x) { return make(x.as<single_float>() .as_exact()); } },
      { typeid(double_float),  [](let const& x) { return make(x.as<double_float>() .as_exact()); } },
      { typeid(ratio),         [](let const& x) { return make(x.as<ratio>()        .as_exact()); } },
      { typeid(exact_integer), [](let const& x) { return make(x.as<exact_integer>().as_exact()); } },
    };

    return resolve(overloads, z);
  };

  auto inexact = [](let const& z)
  {
    static std::unordered_map<
      std::type_index, std::function<object (object const&)>> const overloads
    {
      { typeid(single_float),  [](let const& x) { return make(x.as<single_float>() .as_inexact<decltype(0.0)>()); } },
      { typeid(double_float),  [](let const& x) { return make(x.as<double_float>() .as_inexact<decltype(0.0)>()); } },
      { typeid(ratio),         [](let const& x) { return make(x.as<ratio>()        .as_inexact<decltype(0.0)>()); } },
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
