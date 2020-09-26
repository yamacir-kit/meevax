#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#ifndef MEEVAX_USE_GMP
#define MEEVAX_USE_GMP
#endif

#ifdef MEEVAX_USE_GMP
#include <boost/multiprecision/gmp.hpp>
#else
#include <boost/multiprecision/cpp_int.hpp>
#endif

#include <boost/math/constants/constants.hpp>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/complex.hpp>

namespace meevax { inline namespace kernel
{
  /* ==== Numbers ==============================================================
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
   * ======================================================================== */
  template <typename T>
  struct floating_point;

  #define BOILERPLATE(TYPE)                                                    \
  template <>                                                                  \
  struct floating_point<TYPE>                                                  \
    : public std::numeric_limits<TYPE>                                         \
  {                                                                            \
    using value_type = TYPE;                                                   \
                                                                               \
    value_type value;                                                          \
                                                                               \
    template <typename... Ts>                                                  \
    explicit constexpr floating_point(Ts&&... xs)                              \
      : value { boost::lexical_cast<value_type>(std::forward<decltype(xs)>(xs)...) } \
    {}                                                                         \
                                                                               \
    template <typename T,                                                      \
              typename =                                                       \
                typename std::enable_if<                                       \
                  std::is_convertible<T, value_type>::value                    \
                >::type>                                                       \
    explicit constexpr floating_point(T&& x)                                   \
      : value { x }                                                            \
    {}                                                                         \
                                                                               \
    constexpr operator value_type() const noexcept                             \
    {                                                                          \
      return value;                                                            \
    }                                                                          \
                                                                               \
    auto operator ==(const object&) const -> object;                           \
    auto operator !=(const object&) const -> object;                           \
    auto operator < (const object&) const -> object;                           \
    auto operator <=(const object&) const -> object;                           \
    auto operator > (const object&) const -> object;                           \
    auto operator >=(const object&) const -> object;                           \
                                                                               \
    template <typename T>                                                      \
    auto operator ==(T&& rhs) const noexcept                                   \
    {                                                                          \
      return value == rhs;                                                     \
    }                                                                          \
                                                                               \
    template <typename T>                                                      \
    auto operator !=(T&& rhs) const noexcept                                   \
    {                                                                          \
      return value != rhs;                                                     \
    }                                                                          \
                                                                               \
    auto exact() const noexcept                                                \
    {                                                                          \
      return value == std::trunc(value);                                       \
    }                                                                          \
                                                                               \
    auto to_string() const -> std::string                                      \
    {                                                                          \
      return boost::lexical_cast<std::string>(value);                          \
    }                                                                          \
  }

  // XXX A terrible implementation based on optimistic assumptions.
  BOILERPLATE(float);
  BOILERPLATE(double);

  using most_precise = double;

  #undef BOILERPLATE

  template <typename T>
  auto operator <<(std::ostream& os, const floating_point<T>& rhs) -> decltype(auto)
  {
    if (std::isnan(rhs))
    {
      return os << cyan << "+nan.0" << reset;
    }
    else if (std::isinf(rhs))
    {
      return os << cyan << (0 < rhs.value ? '+' : '-') << "inf.0" << reset;
    }
    else
    {
      return os << cyan << rhs.value << (rhs.exact() ? ".0" : "") << reset;
    }
  }

  struct ratio
    : public virtual pair
  {
  };

  struct exact_integer
  {
    #ifdef MEEVAX_USE_GMP
    using value_type = boost::multiprecision::mpz_int;
    #else
    using value_type = boost::multiprecision::cpp_int;
    #endif

    value_type value;

    template <typename... Ts>
    explicit constexpr exact_integer(Ts&&... xs)
      : value { std::forward<decltype(xs)>(xs)... }
    {}

    auto to_string() const -> std::string
    {
      return value.str();
    }

    operator value_type() const noexcept { return value; }
    operator value_type()       noexcept { return value; }

    auto operator *(const object&) const -> object;
    auto operator +(const object&) const -> object;
    auto operator -(const object&) const -> object;
    auto operator /(const object&) const -> object;

    auto operator ==(const object&) const -> object;
    auto operator !=(const object&) const -> object;

    auto operator < (const object&) const -> object;
    auto operator <=(const object&) const -> object;
    auto operator > (const object&) const -> object;
    auto operator >=(const object&) const -> object;

    auto operator ==(const exact_integer& rhs) const { return value == rhs.value; }
    auto operator !=(const exact_integer& rhs) const { return !(*this == rhs); }

    friend std::ostream& operator<<(std::ostream& os, const exact_integer& x)
    {
      return os << console::cyan << x.value.str() << console::reset;
    }
  };

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

  #define BOILERPLATE(SYMBOL, OPERATION)                                       \
  auto exact_integer::operator SYMBOL(const object& rhs) const -> object       \
  {                                                                            \
    if (!rhs)                                                                  \
    {                                                                          \
      std::stringstream port {};                                               \
      port << "no viable " OPERATION " with " << *this << " and " << rhs;      \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
    else if (rhs.is<floating_point<float>>())                                  \
    {                                                                          \
      return make<boolean>(value SYMBOL static_cast<exact_integer::value_type>(rhs.as<floating_point<float>>().value)); \
    }                                                                          \
    else if (rhs.is<floating_point<double>>())                                 \
    {                                                                          \
      return make<boolean>(value SYMBOL static_cast<exact_integer::value_type>(rhs.as<floating_point<double>>().value)); \
    }                                                                          \
    else if (rhs.is<exact_integer>())                                          \
    {                                                                          \
      return make<boolean>(value SYMBOL rhs.as<exact_integer>().value);        \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      std::stringstream port {};                                               \
      port << "no viable " OPERATION " with " << *this << " and " << rhs;      \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
  } static_assert(true, "semicolon required after this macro")

  BOILERPLATE(==, "equality comparison");
  BOILERPLATE(!=, "inequality comparison");
  BOILERPLATE(<,  "less-than comparison");
  BOILERPLATE(<=, "less-equal comparison");
  BOILERPLATE(>,  "greater-than comparison");
  BOILERPLATE(>=, "greater-equal comparison");

  #undef BOILERPLATE

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
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
