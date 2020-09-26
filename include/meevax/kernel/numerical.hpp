#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <boost/math/constants/constants.hpp>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/floating_point.hpp>
#include <meevax/kernel/ratio.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Numbers --------------------------------------------------------------
   *
   *  number
   *   `-- complex
   *        `-- real
   *             |-- floating-point (IEEE 754)
   *             |    |-- binary  16
   *             |    |-- binary  32 (C++ single float)      = floating_point<float>
   *             |    |-- binary  64 (C++ double float)      = floating_point<double>
   *             |    |-- binary  80 (C++ long double float) = floating_point<long double>
   *             |    `-- binary 128
   *             `-- rational
   *                  |-- fractional
   *                  `-- exact-integer
   *                       |-- multi-precision exact-integer
   *                       `-- fixed precision exact-integer
   *                            |-- signed and unsigned   8  = number<std::u?int8_t>
   *                            |-- signed and unsigned  16  = number<std::u?int16_t>
   *                            |-- signed and unsigned  32  = number<std::u?int32_t>
   *                            |-- signed and unsigned  64  = number<std::u?int64_t>
   *                            `-- signed and unsigned 128  = number<std::u?int128_t>
   *
   * ------------------------------------------------------------------------ */

  /* ---- Arithmetic Operations ------------------------------------------------
   *
   * ┌─────┬─────┬─────┬─────┬
   * │ l\r │ f32 │ f64 │ mpi │
   * ├─────┼─────┼─────┼─────┼
   * │ f32 │  v  │  v  │  v  │
   * ├─────┼─────┼─────┼─────┼
   * │ f64 │  v  │  v  │  v  │
   * ├─────┼─────┼─────┼─────┼
   * │ mpi │  v  │  v  │  v  │
   * ├─────┼─────┼─────┼─────┼
   *
   * ------------------------------------------------------------------------ */

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  auto operator SYMBOL(const floating_point<T>& lhs, const exact_integer& rhs) \
  {                                                                            \
    return floating_point<T>(lhs.value SYMBOL rhs.value.convert_to<T>());      \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);

  #undef BOILERPLATE

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  auto operator SYMBOL(const exact_integer& lhs, const floating_point<T>& rhs) \
  {                                                                            \
    return floating_point<T>(lhs.value.convert_to<T>() SYMBOL rhs.value);      \
  } static_assert(true)

  BOILERPLATE(*);
  BOILERPLATE(+);
  BOILERPLATE(-);
  BOILERPLATE(/);

  #undef BOILERPLATE

  /* ---- Arithmetic Operation Dispatcher --------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */

  #define BOILERPLATE(FLOATING_POINT, SYMBOL, OPERATION)                       \
  auto FLOATING_POINT::operator SYMBOL(const object& rhs) const -> object      \
  {                                                                            \
    if (rhs)                                                                   \
    {                                                                          \
      if (rhs.is<exact_integer>())                                             \
      {                                                                        \
        return make(*this SYMBOL rhs.as<exact_integer>());                     \
      }                                                                        \
      else if (rhs.is<single_float>())                                         \
      {                                                                        \
        return make(*this SYMBOL rhs.as<single_float>());                      \
      }                                                                        \
      else if (rhs.is<double_float>())                                         \
      {                                                                        \
        return make(*this SYMBOL rhs.as<double_float>());                      \
      }                                                                        \
    }                                                                          \
                                                                               \
    std::stringstream ss {};                                                   \
    ss << "no viable operation '" #OPERATION "' with " << *this << " and " << rhs; \
    throw std::logic_error { ss.str() };                                       \
  } static_assert(true)

  BOILERPLATE(single_float, *, multiplies);
  BOILERPLATE(single_float, +, plus);
  BOILERPLATE(single_float, -, minus);
  BOILERPLATE(single_float, /, divides);

  BOILERPLATE(double_float, *, multiplies);
  BOILERPLATE(double_float, +, plus);
  BOILERPLATE(double_float, -, minus);
  BOILERPLATE(double_float, /, divides);

  #undef BOILERPLATE

  #define BOILERPLATE(SYMBOL, OPERATION)                                       \
  auto exact_integer::operator SYMBOL(const object& rhs) const -> object       \
  {                                                                            \
    if (rhs)                                                                   \
    {                                                                          \
      if (rhs.is<exact_integer>())                                             \
      {                                                                        \
        return make(*this SYMBOL rhs.as<exact_integer>());                     \
      }                                                                        \
      else if (rhs.is<single_float>())                                         \
      {                                                                        \
        return make(*this SYMBOL rhs.as<single_float>());                      \
      }                                                                        \
      else if (rhs.is<double_float>())                                         \
      {                                                                        \
        return make(*this SYMBOL rhs.as<double_float>());                      \
      }                                                                        \
    }                                                                          \
                                                                               \
    std::stringstream ss {};                                                   \
    ss << "no viable operation '" #OPERATION "' with " << *this << " and " << rhs; \
    throw std::logic_error { ss.str() };                                       \
  } static_assert(true)

  BOILERPLATE(*, multiplies);
  BOILERPLATE(+, plus);
  BOILERPLATE(-, minus);
  BOILERPLATE(/, divides);

  #undef BOILERPLATE

  /* ---- Arithmetic Comparisons -----------------------------------------------
   *
   * ┌─────┬─────┬─────┬─────┬
   * │ l\r │ f32 │ f64 │ mpi │
   * ├─────┼─────┼─────┼─────┼
   * │ f32 │     │     │     │
   * ├─────┼─────┼─────┼─────┼
   * │ f64 │     │     │     │
   * ├─────┼─────┼─────┼─────┼
   * │ mpi │     │     │     │
   * ├─────┼─────┼─────┼─────┼
   *
   * ------------------------------------------------------------------------ */

  #define BOILERPLATE(SYMBOL)                                                  \
  template <typename T>                                                        \
  auto operator SYMBOL(const exact_integer& lhs, const floating_point<T>& rhs) \
  {                                                                            \
    return lhs SYMBOL static_cast<exact_integer>(rhs.value);                   \
  } static_assert(true)

  BOILERPLATE(!=);
  BOILERPLATE(<);
  BOILERPLATE(<=);
  BOILERPLATE(==);
  BOILERPLATE(>);
  BOILERPLATE(>=);

  #undef BOILERPLATE

  /* ---- Arithmetic Comparison Dispatcher -------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */

  #define BOILERPLATE(TYPE, SYMBOL, OPERATION)                                 \
  auto TYPE::operator SYMBOL(const object& rhs) const -> object                \
  {                                                                            \
    if (!rhs)                                                                  \
    {                                                                          \
      std::stringstream port {};                                               \
      port << "no viable " OPERATION " with " << *this << " and " << rhs;      \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
    else if (rhs.is<floating_point<float>>())                                  \
    {                                                                          \
      return make<boolean>(value SYMBOL rhs.as<floating_point<float>>().value); \
    }                                                                          \
    else if (rhs.is<floating_point<double>>())                                 \
    {                                                                          \
      return make<boolean>(value SYMBOL rhs.as<floating_point<double>>().value); \
    }                                                                          \
    else if (rhs.is<exact_integer>())                                          \
    {                                                                          \
      return static_cast<exact_integer>(value) SYMBOL rhs;                     \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      std::stringstream port {};                                               \
      port << "no viable " OPERATION " with " << *this << " and " << rhs;      \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
  } static_assert(true, "semicolon required after this macro")

  BOILERPLATE(single_float, ==, "equality comparison");
  BOILERPLATE(single_float, !=, "inequality comparison");
  BOILERPLATE(single_float, <,  "less-than comparison");
  BOILERPLATE(single_float, <=, "less-equal comparison");
  BOILERPLATE(single_float, >,  "greater-than comparison");
  BOILERPLATE(single_float, >=, "greater-equal comparison");

  BOILERPLATE(double_float, ==, "equality comparison");
  BOILERPLATE(double_float, !=, "inequality comparison");
  BOILERPLATE(double_float, <,  "less-than comparison");
  BOILERPLATE(double_float, <=, "less-equal comparison");
  BOILERPLATE(double_float, >,  "greater-than comparison");
  BOILERPLATE(double_float, >=, "greater-equal comparison");

  #undef BOILERPLATE

  #define BOILERPLATE(SYMBOL, OPERATION)                                       \
  auto exact_integer::operator SYMBOL(const object& rhs) const -> object       \
  {                                                                            \
    if (rhs)                                                                   \
    {                                                                          \
      if (rhs.is<exact_integer>())                                             \
      {                                                                        \
        return make<boolean>(*this SYMBOL rhs.as<exact_integer>());            \
      }                                                                        \
      else if (rhs.is<floating_point<float>>())                                \
      {                                                                        \
        return make<boolean>(*this SYMBOL rhs.as<floating_point<float>>());    \
      }                                                                        \
      else if (rhs.is<floating_point<double>>())                               \
      {                                                                        \
        return make<boolean>(*this SYMBOL rhs.as<floating_point<double>>());   \
      }                                                                        \
    }                                                                          \
                                                                               \
    std::stringstream port {};                                                 \
    port << "no viable operation '" #OPERATION "' with " << *this << " and " << rhs; \
    throw std::logic_error { port.str() };                                     \
  } static_assert(true, "semicolon required after this macro")

  BOILERPLATE(!=, not_equal_to);
  BOILERPLATE(<,  less);
  BOILERPLATE(<=, less_equal);
  BOILERPLATE(==, equal_to);
  BOILERPLATE(>,  greater);
  BOILERPLATE(>=, greater_equal);

  #undef BOILERPLATE
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
