#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <boost/math/constants/constants.hpp>

#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/floating_point.hpp>
#include <meevax/kernel/ratio.hpp>

#include <typeindex>

namespace meevax
{
inline namespace kernel
{
  [[deprecated]]
  auto to_inexact(const exact_integer&) -> default_float; // TODO use exact_integer::as_inexact

  [[deprecated]]
  auto to_inexact(const ratio&) -> default_float;

  template <typename T>
  [[deprecated]]
  constexpr auto to_inexact(const floating_point<T>& datum)
  {
    return datum;
  }

  /* ---- Generic ----------------------------------------------------------- */

  auto exact = [](const object& z)
  {
    #define BOILERPLATE(TYPE)                                                  \
    {                                                                          \
      typeid(TYPE), [](auto&& z)                                               \
      {                                                                        \
        return z.template as<TYPE>().as_exact();                               \
      }                                                                        \
    }

    static const std::unordered_map<
      std::type_index,
      std::function<exact_integer (const object&)>
    >
    match
    {
      BOILERPLATE(single_float),
      BOILERPLATE(double_float),
      // BOILERPLATE(ratio),
      BOILERPLATE(exact_integer),
    };

    #undef BOILERPLATE

    return match.at(z.type())(z);
  };

  auto inexact = [](let const& z)
  {
    static const std::unordered_map<
      std::type_index,
      std::function<default_float (const object&)>
    >
    match
    {
      { typeid(single_float),  [](let const& x) { return x.as<single_float>() .as_inexact<decltype(0.0)>(); } },
      { typeid(double_float),  [](let const& x) { return x.as<double_float>() .as_inexact<decltype(0.0)>(); } },
      { typeid(ratio),         [](let const& x) { return x.as<ratio>()        .as_inexact<decltype(0.0)>(); } },
      { typeid(exact_integer), [](let const& x) { return x.as<exact_integer>().as_inexact<decltype(0.0)>(); } },
    };

    return match.at(z.type())(z);
  };

  auto is_nan = [](const object& x)
  {
    if (x.is<null>())
    {
      return false;
    }
    else if (x.is<single_float>())
    {
      return std::isnan(x.as<single_float>());
    }
    else if (x.is<double_float>())
    {
      return std::isnan(x.as<double_float>());
    }
    else
    {
     return false;
    }
  };

  /* ---- Arithmetic Operations ------------------------------------------------
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
   * │  ratio  │    f    │    f    │    f    │   t   │    f    │
   * ├─────────┼─────────┼─────────┼─────────┼───────┼─────────┼
   * │ complex │    f    │    f    │    f    │   f   │    f    │
   * ├─────────┼─────────┼─────────┼─────────┼───────┼─────────┼
   *
   * ------------------------------------------------------------------------ */

  template <typename T> constexpr auto operator * (floating_point<T> const& a, ratio const& b) { return a *  b.as_inexact<T>(); }
  template <typename T> constexpr auto operator + (floating_point<T> const& a, ratio const& b) { return a +  b.as_inexact<T>(); }
  template <typename T> constexpr auto operator - (floating_point<T> const& a, ratio const& b) { return a -  b.as_inexact<T>(); }
  template <typename T> constexpr auto operator / (floating_point<T> const& a, ratio const& b) { return a /  b.as_inexact<T>(); }
  template <typename T> constexpr auto operator % (floating_point<T> const& a, ratio const& b) { return a %  b.as_inexact<T>(); }
  template <typename T> constexpr auto operator !=(floating_point<T> const& a, ratio const& b) { return a != b.as_inexact<T>(); }
  template <typename T> constexpr auto operator < (floating_point<T> const& a, ratio const& b) { return a <  b.as_inexact<T>(); }
  template <typename T> constexpr auto operator <=(floating_point<T> const& a, ratio const& b) { return a <= b.as_inexact<T>(); }
  template <typename T> constexpr auto operator ==(floating_point<T> const& a, ratio const& b) { return a == b.as_inexact<T>(); }
  template <typename T> constexpr auto operator > (floating_point<T> const& a, ratio const& b) { return a >  b.as_inexact<T>(); }
  template <typename T> constexpr auto operator >=(floating_point<T> const& a, ratio const& b) { return a >= b.as_inexact<T>(); }

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
      if (rhs.is<exact_integer>())                                             \
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
