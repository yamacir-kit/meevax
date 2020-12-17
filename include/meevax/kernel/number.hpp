#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <boost/math/constants/constants.hpp>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/floating_point.hpp>
#include <meevax/kernel/ratio.hpp>

#include <typeindex>

namespace meevax
{
inline namespace kernel
{
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

  template <typename T> auto operator * (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() *  b; }
  template <typename T> auto operator + (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() +  b; }
  template <typename T> auto operator - (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() -  b; }
  template <typename T> auto operator / (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() /  b; }
  template <typename T> auto operator % (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() %  b; }
  template <typename T> auto operator !=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() != b; }
  template <typename T> auto operator < (exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() <  b; }
  template <typename T> auto operator <=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() <= b; }
  template <typename T> auto operator ==(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() == b; }
  template <typename T> auto operator > (exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() >  b; }
  template <typename T> auto operator >=(exact_integer const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() >= b; }

  auto operator !=(ratio const&, object const&) -> bool;
  auto operator < (ratio const&, object const&) -> bool;
  auto operator <=(ratio const&, object const&) -> bool;
  auto operator ==(ratio const&, object const&) -> bool;
  auto operator > (ratio const&, object const&) -> bool;
  auto operator >=(ratio const&, object const&) -> bool;

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

  template <typename T>  auto operator !=(ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() != b; }
  template <typename T>  auto operator < (ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() <  b; }
  template <typename T>  auto operator <=(ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() <= b; }
  template <typename T>  auto operator ==(ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() == b; }
  template <typename T>  auto operator > (ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() >  b; }
  template <typename T>  auto operator >=(ratio const& a, floating_point<T> const& b) -> boolean { return a.as_inexact<T>() >= b; }

  template <typename T>  auto operator * (floating_point<T> const&, object const&) -> object;
  template <typename T>  auto operator + (floating_point<T> const&, object const&) -> object;
  template <typename T>  auto operator - (floating_point<T> const&, object const&) -> object;
  template <typename T>  auto operator / (floating_point<T> const&, object const&) -> object;
  template <typename T>  auto operator % (floating_point<T> const&, object const&) -> object;
  template <typename T>  auto operator ==(floating_point<T> const&, object const&) -> bool;
  template <typename T>  auto operator !=(floating_point<T> const&, object const&) -> bool;
  template <typename T>  auto operator < (floating_point<T> const&, object const&) -> bool;
  template <typename T>  auto operator <=(floating_point<T> const&, object const&) -> bool;
  template <typename T>  auto operator > (floating_point<T> const&, object const&) -> bool;
  template <typename T>  auto operator >=(floating_point<T> const&, object const&) -> bool;

  template <typename T>  auto operator * (floating_point<T> const& a, exact_integer const& b) { return a *  b.as_inexact<T>(); }
  template <typename T>  auto operator + (floating_point<T> const& a, exact_integer const& b) { return a +  b.as_inexact<T>(); }
  template <typename T>  auto operator - (floating_point<T> const& a, exact_integer const& b) { return a -  b.as_inexact<T>(); }
  template <typename T>  auto operator / (floating_point<T> const& a, exact_integer const& b) { return a /  b.as_inexact<T>(); }
  template <typename T>  auto operator % (floating_point<T> const& a, exact_integer const& b) { return a %  b.as_inexact<T>(); }
  template <typename T>  auto operator !=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a != b.as_inexact<T>(); }
  template <typename T>  auto operator < (floating_point<T> const& a, exact_integer const& b) -> boolean { return a <  b.as_inexact<T>(); }
  template <typename T>  auto operator <=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a <= b.as_inexact<T>(); }
  template <typename T>  auto operator ==(floating_point<T> const& a, exact_integer const& b) -> boolean { return a == b.as_inexact<T>(); }
  template <typename T>  auto operator > (floating_point<T> const& a, exact_integer const& b) -> boolean { return a >  b.as_inexact<T>(); }
  template <typename T>  auto operator >=(floating_point<T> const& a, exact_integer const& b) -> boolean { return a >= b.as_inexact<T>(); }

  template <typename T>  auto operator * (floating_point<T> const& a, ratio const& b) { return a *  b.as_inexact<T>(); }
  template <typename T>  auto operator + (floating_point<T> const& a, ratio const& b) { return a +  b.as_inexact<T>(); }
  template <typename T>  auto operator - (floating_point<T> const& a, ratio const& b) { return a -  b.as_inexact<T>(); }
  template <typename T>  auto operator / (floating_point<T> const& a, ratio const& b) { return a /  b.as_inexact<T>(); }
  template <typename T>  auto operator % (floating_point<T> const& a, ratio const& b) { return a %  b.as_inexact<T>(); }
  template <typename T>  auto operator !=(floating_point<T> const& a, ratio const& b) -> boolean { return a != b.as_inexact<T>(); }
  template <typename T>  auto operator < (floating_point<T> const& a, ratio const& b) -> boolean { return a <  b.as_inexact<T>(); }
  template <typename T>  auto operator <=(floating_point<T> const& a, ratio const& b) -> boolean { return a <= b.as_inexact<T>(); }
  template <typename T>  auto operator ==(floating_point<T> const& a, ratio const& b) -> boolean { return a == b.as_inexact<T>(); }
  template <typename T>  auto operator > (floating_point<T> const& a, ratio const& b) -> boolean { return a >  b.as_inexact<T>(); }
  template <typename T>  auto operator >=(floating_point<T> const& a, ratio const& b) -> boolean { return a >= b.as_inexact<T>(); }

  template <typename T, typename U> auto operator * (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value * b.value); }
  template <typename T, typename U> auto operator + (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value + b.value); }
  template <typename T, typename U> auto operator - (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value - b.value); }
  template <typename T, typename U> auto operator / (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value / b.value); }
  template <typename T, typename U> auto operator % (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(std::fmod(a.value, b.value)); }
  template <typename T, typename U> auto operator !=(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value != b.value; }
  template <typename T, typename U> auto operator < (floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value <  b.value; }
  template <typename T, typename U> auto operator <=(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value <= b.value; }
  template <typename T, typename U> auto operator ==(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value == b.value; }
  template <typename T, typename U> auto operator > (floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value >  b.value; }
  template <typename T, typename U> auto operator >=(floating_point<T> const& a, floating_point<U> const& b) -> boolean { return a.value >= b.value; }

  template <typename T>
  T resolve(std::unordered_map<std::type_index, std::function<T (object const&)>> const& overload, object const& x)
  {
    if (auto const iter { overload.find(x.type()) }; iter != std::end(overload))
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
    static const std::unordered_map<std::type_index, std::function<object (object const&)>> overload
    {
      { typeid(single_float),  [](let const& x) { return make(x.as<single_float>() .as_exact()); } },
      { typeid(double_float),  [](let const& x) { return make(x.as<double_float>() .as_exact()); } },
      { typeid(ratio),         [](let const& x) { return make(x.as<ratio>()        .as_exact()); } },
      { typeid(exact_integer), [](let const& x) { return make(x.as<exact_integer>().as_exact()); } },
    };

    return resolve(overload, z);
  };

  auto inexact = [](let const& z)
  {
    static const std::unordered_map<std::type_index, std::function<default_float (object const&)>> overload
    {
      { typeid(single_float),  [](let const& x) { return x.as<single_float>() .as_inexact<decltype(0.0)>(); } },
      { typeid(double_float),  [](let const& x) { return x.as<double_float>() .as_inexact<decltype(0.0)>(); } },
      { typeid(ratio),         [](let const& x) { return x.as<ratio>()        .as_inexact<decltype(0.0)>(); } },
      { typeid(exact_integer), [](let const& x) { return x.as<exact_integer>().as_inexact<decltype(0.0)>(); } },
    };

    return resolve(overload, z);
  };

  auto is_nan = [](object const& x)
  {
    static std::unordered_map<std::type_index, std::function<bool (object const&)>> const overload
    {
      { typeid(null),         [](let const&  ) { return false; } },
      { typeid(single_float), [](let const& x) { return std::isnan(x.as<single_float>()); } },
      { typeid(double_float), [](let const& x) { return std::isnan(x.as<double_float>()); } },
    };

    return resolve(overload, x);
  };

  /* ---- Arithmetic Operation Dispatcher --------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  let operator SYMBOL(const floating_point<T>& lhs, const object& rhs)         \
  {                                                                            \
    if (rhs)                                                                   \
    {                                                                          \
      if (rhs.is<ratio>())                                                     \
      {                                                                        \
        return make(lhs SYMBOL rhs.as<ratio>());                               \
      }                                                                        \
      else if (rhs.is<exact_integer>())                                        \
      {                                                                        \
        return make(lhs SYMBOL rhs.as<exact_integer>());                       \
      }                                                                        \
      else if (rhs.is<single_float>())                                         \
      {                                                                        \
        return make(lhs SYMBOL rhs.as<single_float>());                        \
      }                                                                        \
      else if (rhs.is<double_float>())                                         \
      {                                                                        \
        return make(lhs SYMBOL rhs.as<double_float>());                        \
      }                                                                        \
    }                                                                          \
                                                                               \
    throw error("no viable operation '" #SYMBOL " with ", lhs, " and ", rhs);  \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);
  BOILERPLATE(%);

  #undef BOILERPLATE

  /* ---- Arithmetic Comparisons -----------------------------------------------
   *
   * TODO CONSIDER EPSILON
   *
   * ┌─────────┬─────────┬─────────┬─────────┬───────┬─────────┬
   * │ LHS\RHS │   f32   │   f64   │   big   │ ratio │ complex │
   * ├─────────┼─────────┼─────────┼─────────┼───────┼─────────┼
   * │   f32   │    t    │    t    │    t    │   t   │    f    │
   * ├─────────┼─────────┼─────────┼─────────┼───────┼─────────┼
   * │   f64   │    t    │    t    │    t    │   t   │    f    │
   * ├─────────┼─────────┼─────────┼─────────┼───────┼─────────┼
   * │   big   │    t    │    t    │    t    │   t   │    f    │
   * ├─────────┼─────────┼─────────┼─────────┼───────┼─────────┼
   * │  ratio  │    f    │    f    │    f    │   f   │    f    │
   * ├─────────┼─────────┼─────────┼─────────┼───────┼─────────┼
   * │ complex │    f    │    f    │    f    │   f   │    f    │
   * ├─────────┼─────────┼─────────┼─────────┼───────┼─────────┼
   *
   * ------------------------------------------------------------------------ */

  /* ---- Arithmetic Comparison Dispatcher ---------------------------------- */

  #define BOILERPLATE(NUMBER, SYMBOL)                                          \
  auto operator SYMBOL(const NUMBER& lhs, const object& rhs) -> bool           \
  {                                                                            \
    if (rhs)                                                                   \
    {                                                                          \
      if (rhs.is<exact_integer>())                                             \
      {                                                                        \
        return lhs SYMBOL rhs.as<exact_integer>();                             \
      }                                                                        \
      else if (rhs.is<ratio>())                                                \
      {                                                                        \
        return lhs SYMBOL rhs.as<ratio>();                                     \
      }                                                                        \
      else if (rhs.is<single_float>())                                         \
      {                                                                        \
        return lhs SYMBOL rhs.as<single_float>();                              \
      }                                                                        \
      else if (rhs.is<double_float>())                                         \
      {                                                                        \
        return lhs SYMBOL rhs.as<double_float>();                              \
      }                                                                        \
    }                                                                          \
                                                                               \
    throw error("no viable operation '" #SYMBOL " with ", lhs, " and ", rhs);  \
  } static_assert(true)

  template <typename T> BOILERPLATE(floating_point<T>, !=);
  template <typename T> BOILERPLATE(floating_point<T>, < );
  template <typename T> BOILERPLATE(floating_point<T>, <=);
  template <typename T> BOILERPLATE(floating_point<T>, ==);
  template <typename T> BOILERPLATE(floating_point<T>, > );
  template <typename T> BOILERPLATE(floating_point<T>, >=);

  #undef BOILERPLATE
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
