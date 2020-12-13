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
  /* ---- Multi-Precision Exact-Integer ------------------------------------- */

  auto to_inexact(const exact_integer&) -> default_float; // TODO use exact_integer::as_inexact

  /* ---- Ratio ------------------------------------------------------------- */

  auto to_inexact(const ratio&) -> default_float;

  /* ---- Floating-Point Numbers -------------------------------------------- */

  template <typename T>
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

  auto inexact = [](const object& z)
  {
    #define BOILERPLATE(TYPE)                                                  \
    {                                                                          \
      typeid(TYPE), [](let const& z)                                           \
      {                                                                        \
        return to_inexact(z.as<TYPE>());                                       \
      }                                                                        \
    }

    static const std::unordered_map<
      std::type_index,
      std::function<default_float (const object&)>
    >
    match
    {
      // BOILERPLATE(single_float),
      BOILERPLATE(double_float),
      BOILERPLATE(ratio),
      BOILERPLATE(exact_integer),
    };

    #undef BOILERPLATE

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

  /* ---- Multi-Precision Exact-Integer ------------------------------------- */

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  auto operator SYMBOL(const exact_integer& lhs, const floating_point<T>& rhs) \
  {                                                                            \
    return floating_point(to_inexact(lhs) SYMBOL rhs);                         \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);
  BOILERPLATE(%);

  #undef BOILERPLATE

  auto operator +(const exact_integer&, const ratio&) -> ratio;
  auto operator -(const exact_integer&, const ratio&) -> ratio;
  auto operator *(const exact_integer&, const ratio&) -> ratio;
  auto operator /(const exact_integer&, const ratio&) -> ratio;
  auto operator %(const exact_integer&, const ratio&) -> ratio;

  /* ---- Ratio ------------------------------------------------------------- */


  /* ---- Floating-Point Numbers -------------------------------------------- */

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  auto operator SYMBOL(const floating_point<T>& lhs, const exact_integer& rhs) \
  {                                                                            \
    return floating_point(lhs SYMBOL to_inexact(rhs));                         \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);
  BOILERPLATE(%);

  #undef BOILERPLATE

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  auto operator SYMBOL(const floating_point<T>& lhs, const ratio& rhs)         \
  {                                                                            \
    return floating_point(lhs SYMBOL to_inexact(rhs));                         \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);
  BOILERPLATE(%);

  #undef BOILERPLATE

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

  let operator +(const exact_integer&, const object&);
  let operator -(const exact_integer&, const object&);
  let operator *(const exact_integer&, const object&);
  let operator /(const exact_integer&, const object&);
  let operator %(const exact_integer&, const object&);

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

  /* ---- Multi-Precision Exact-Integer --------------------------------------*/

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  auto operator SYMBOL(const exact_integer& lhs, const floating_point<T>& rhs) \
  {                                                                            \
    return lhs.value.convert_to<T>() SYMBOL rhs.value;                         \
  } static_assert(true)

  BOILERPLATE(!=);
  BOILERPLATE(<);
  BOILERPLATE(<=);
  BOILERPLATE(==);
  BOILERPLATE(>);
  BOILERPLATE(>=);

  #undef BOILERPLATE

  /* ---- Ratio ------------------------------------------------------------- */

  auto operator !=(const ratio&, const exact_integer&) -> bool;
  auto operator < (const ratio&, const exact_integer&) -> bool;
  auto operator <=(const ratio&, const exact_integer&) -> bool;
  auto operator ==(const ratio&, const exact_integer&) -> bool;
  auto operator > (const ratio&, const exact_integer&) -> bool;
  auto operator >=(const ratio&, const exact_integer&) -> bool;

  #undef BOILERPLATE

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  auto operator SYMBOL(const ratio& lhs, const floating_point<T>& rhs)         \
  {                                                                            \
    return to_inexact(lhs) SYMBOL rhs;                                         \
  } static_assert(true)

  BOILERPLATE(!=);
  BOILERPLATE(<);
  BOILERPLATE(<=);
  BOILERPLATE(==);
  BOILERPLATE(>);
  BOILERPLATE(>=);

  #undef BOILERPLATE

  /* ---- Floating-Point Number --------------------------------------------- */

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  auto operator SYMBOL(const floating_point<T>& lhs, const ratio& rhs)         \
  {                                                                            \
    return lhs SYMBOL to_inexact(rhs);                                         \
  } static_assert(true)

  BOILERPLATE(!=);
  BOILERPLATE(<);
  BOILERPLATE(<=);
  BOILERPLATE(==);
  BOILERPLATE(>);
  BOILERPLATE(>=);

  #undef BOILERPLATE

  /* ---- Arithmetic Comparison Dispatcher ---------------------------------- */

  auto operator !=(const ratio&, const object&) -> bool;
  auto operator < (const ratio&, const object&) -> bool;
  auto operator <=(const ratio&, const object&) -> bool;
  auto operator ==(const ratio&, const object&) -> bool;
  auto operator > (const ratio&, const object&) -> bool;
  auto operator >=(const ratio&, const object&) -> bool;

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
