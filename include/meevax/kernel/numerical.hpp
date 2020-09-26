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
  #define BOILERPLATE(SYMBOL, OPERATION)                                       \
  auto exact_integer::operator SYMBOL(const object& rhs) const -> object       \
  {                                                                            \
    if (!rhs)                                                                  \
    {                                                                          \
      std::stringstream ss {};                                                 \
      ss << "no viable " OPERATION " with " << *this << " and " << rhs;        \
      throw std::logic_error { ss.str() };                                     \
    }                                                                          \
    else if (rhs.is<floating_point<float>>())                                  \
    {                                                                          \
      const exact_integer::value_type result { value SYMBOL static_cast<exact_integer::value_type>(rhs.as<floating_point<float>>().value) }; \
      return make<floating_point<float>>(result.convert_to<floating_point<float>::value_type>()); \
    }                                                                          \
    else if (rhs.is<floating_point<double>>())                                 \
    {                                                                          \
      const exact_integer::value_type result { value SYMBOL static_cast<exact_integer::value_type>(rhs.as<floating_point<double>>().value) }; \
      return make<floating_point<double>>(result.convert_to<double>());        \
    }                                                                          \
    else if (rhs.is<exact_integer>())                                          \
    {                                                                          \
      return make<exact_integer>(value SYMBOL rhs.as<exact_integer>().value);  \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      std::stringstream ss {};                                                 \
      ss << "no viable " OPERATION " with " << *this << " and " << rhs;        \
      throw std::logic_error { ss.str() };                                     \
    }                                                                          \
  } static_assert(true, "semicolon required after this macro")

  BOILERPLATE(*, "multiplication");
  BOILERPLATE(+, "addition");
  BOILERPLATE(-, "subtraction");
  BOILERPLATE(/, "division");

  #undef BOILERPLATE

  // XXX vs exact_integer comparison is maybe incorrect!
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

  BOILERPLATE(floating_point<float>, ==, "equality comparison");
  BOILERPLATE(floating_point<float>, !=, "inequality comparison");
  BOILERPLATE(floating_point<float>, <,  "less-than comparison");
  BOILERPLATE(floating_point<float>, <=, "less-equal comparison");
  BOILERPLATE(floating_point<float>, >,  "greater-than comparison");
  BOILERPLATE(floating_point<float>, >=, "greater-equal comparison");

  BOILERPLATE(floating_point<double>, ==, "equality comparison");
  BOILERPLATE(floating_point<double>, !=, "inequality comparison");
  BOILERPLATE(floating_point<double>, <,  "less-than comparison");
  BOILERPLATE(floating_point<double>, <=, "less-equal comparison");
  BOILERPLATE(floating_point<double>, >,  "greater-than comparison");
  BOILERPLATE(floating_point<double>, >=, "greater-equal comparison");

  #undef BOILERPLATE

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
