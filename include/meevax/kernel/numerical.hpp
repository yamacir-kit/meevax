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

namespace meevax { inline namespace kernel
{
  /* ==== Numbers ==============================================================
   *
   *  number
   *   `-- complex
   *        `-- real
   *             |-- decimal (IEEE 754)
   *             |    |-- binary  16
   *             |    |-- binary  32 (C++ single float)      = number<float>
   *             |    |-- binary  64 (C++ double float)      = number<double>
   *             |    |-- binary  80 (C++ long double float) = number<long double>
   *             |    `-- binary 128
   *             `-- rational
   *                  |-- fractional
   *                  `-- integral
   *                       |-- multi-precision integral
   *                       `-- fixed precision integral
   *                            |-- signed and unsigned   8  = number<std::u?int8_t>
   *                            |-- signed and unsigned  16  = number<std::u?int16_t>
   *                            |-- signed and unsigned  32  = number<std::u?int32_t>
   *                            |-- signed and unsigned  64  = number<std::u?int64_t>
   *                            `-- signed and unsigned 128  = number<std::u?int128_t>
   *
   * ======================================================================== */
  struct complex
    : public virtual pair
  {
    auto real() const noexcept -> decltype(auto) { return car(*this); }
    auto real()       noexcept -> decltype(auto) { return car(*this); }

    auto imag() const noexcept -> decltype(auto) { return cdr(*this); }
    auto imag()       noexcept -> decltype(auto) { return cdr(*this); }

    // friend auto operator +(const complex& lhs, const complex& rhs)
    // {
    //   return
    //     make<complex>(
    //       lhs.real() + rhs.real(),
    //       lhs.imag() + rhs.imag());
    // }
    //
    // template <typename T>
    // friend auto operator +(const complex& lhs, T&& rhs)
    // {
    //   return
    //     make<complex>(
    //       lhs.real() + rhs,
    //       lhs.imag());
    // }

    friend std::ostream& operator<<(std::ostream& os, const complex& z)
    {
      return os << cyan << z.real() << (0 < z.imag() ? '+' : '-') << z.imag() << "i" << reset;
    }
  };

  template <auto Bits>
  struct decimal;

  #define boilerplate(BITS, TYPE, CONVERT)                                     \
  template <>                                                                  \
  struct decimal<BITS>                                                         \
    : public std::numeric_limits<TYPE>                                         \
  {                                                                            \
    using value_type = TYPE;                                                   \
                                                                               \
    value_type value;                                                          \
                                                                               \
    template <typename... Ts>                                                  \
    explicit constexpr decimal(Ts&&... xs)                                     \
      : value { CONVERT(std::forward<decltype(xs)>(xs)...) }                   \
    {}                                                                         \
                                                                               \
    template <typename T,                                                      \
              typename =                                                       \
                typename std::enable_if<                                       \
                  std::is_convertible<T, value_type>::value                    \
                >::type>                                                       \
    explicit constexpr decimal(T&& x)                                          \
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
  boilerplate(32, float, std::stof);
  boilerplate(64, double, std::stod);

  constexpr auto most_precise = 64;

  #undef boilerplate

  template <auto B>
  auto operator<<(std::ostream& os, const decimal<B>& x) -> decltype(os)
  {
    if (std::isnan(x))
    {
      return os << cyan << "+nan.0" << reset;
    }
    else if (std::isinf(x))
    {
      return os << cyan << (0 < x.value ? '+' : '-') << "inf.0" << reset;
    }
    else
    {
      return os << cyan << x.value << (x.exact() ? ".0" : "") << reset;
    }
  }

  struct fractional
    : public virtual pair
  {
  };

  struct integral
  {
    #ifdef MEEVAX_USE_GMP
    using value_type = boost::multiprecision::mpz_int;
    #else
    using value_type = boost::multiprecision::cpp_int;
    #endif

    value_type value;

    template <typename... Ts>
    explicit constexpr integral(Ts&&... xs)
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

    auto operator ==(const integral& rhs) const { return value == rhs.value; }
    auto operator !=(const integral& rhs) const { return !(*this == rhs); }

    friend std::ostream& operator<<(std::ostream& os, const integral& x)
    {
      return os << console::cyan << x.value.str() << console::reset;
    }
  };

  // XXX vs integral comparison is maybe incorrect!
  #define boilerplate(TYPE, SYMBOL, OPERATION)                                 \
  auto TYPE::operator SYMBOL(const object& rhs) const -> object                \
  {                                                                            \
    if (!rhs)                                                                  \
    {                                                                          \
      std::stringstream port {};                                               \
      port << "no viable " OPERATION " with " << *this << " and " << rhs;      \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
    else if (rhs.is<decimal<32>>())                                            \
    {                                                                          \
      return make<boolean>(value SYMBOL rhs.as<decimal<32>>().value);          \
    }                                                                          \
    else if (rhs.is<decimal<64>>())                                            \
    {                                                                          \
      return make<boolean>(value SYMBOL rhs.as<decimal<64>>().value);          \
    }                                                                          \
    else if (rhs.is<integral>())                                                \
    {                                                                          \
      return static_cast<integral>(value) SYMBOL rhs;                           \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      std::stringstream port {};                                               \
      port << "no viable " OPERATION " with " << *this << " and " << rhs;      \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
  } static_assert(true, "semicolon required after this macro")

  boilerplate(decimal<32>, ==, "equality comparison");
  boilerplate(decimal<32>, !=, "inequality comparison");
  boilerplate(decimal<32>, <,  "less-than comparison");
  boilerplate(decimal<32>, <=, "less-equal comparison");
  boilerplate(decimal<32>, >,  "greater-than comparison");
  boilerplate(decimal<32>, >=, "greater-equal comparison");

  boilerplate(decimal<64>, ==, "equality comparison");
  boilerplate(decimal<64>, !=, "inequality comparison");
  boilerplate(decimal<64>, <,  "less-than comparison");
  boilerplate(decimal<64>, <=, "less-equal comparison");
  boilerplate(decimal<64>, >,  "greater-than comparison");
  boilerplate(decimal<64>, >=, "greater-equal comparison");

  #undef boilerplate

  #define boilerplate(SYMBOL, OPERATION)                                       \
  auto integral::operator SYMBOL(const object& rhs) const -> object             \
  {                                                                            \
    if (!rhs)                                                                  \
    {                                                                          \
      std::stringstream port {};                                               \
      port << "no viable " OPERATION " with " << *this << " and " << rhs;      \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
    else if (rhs.is<decimal<32>>())                                            \
    {                                                                          \
      return make<boolean>(value SYMBOL static_cast<integral::value_type>(rhs.as<decimal<32>>().value)); \
    }                                                                          \
    else if (rhs.is<decimal<64>>())                                            \
    {                                                                          \
      return make<boolean>(value SYMBOL static_cast<integral::value_type>(rhs.as<decimal<64>>().value)); \
    }                                                                          \
    else if (rhs.is<integral>())                                                \
    {                                                                          \
      return make<boolean>(value SYMBOL rhs.as<integral>().value);              \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      std::stringstream port {};                                               \
      port << "no viable " OPERATION " with " << *this << " and " << rhs;      \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
  } static_assert(true, "semicolon required after this macro")

  boilerplate(==, "equality comparison");
  boilerplate(!=, "inequality comparison");
  boilerplate(<,  "less-than comparison");
  boilerplate(<=, "less-equal comparison");
  boilerplate(>,  "greater-than comparison");
  boilerplate(>=, "greater-equal comparison");

  #undef boilerplate

  #define DEFINE_BINARY_ARITHMETIC_INTEGER(SYMBOL, OPERATION)                  \
  auto integral::operator SYMBOL(const object& rhs) const -> object             \
  {                                                                            \
    if (!rhs)                                                                  \
    {                                                                          \
      std::stringstream ss {};                                                 \
      ss << "no viable " OPERATION " with " << *this << " and " << rhs;        \
      throw std::logic_error { ss.str() };                                     \
    }                                                                          \
    else if (rhs.is<decimal<32>>())                                            \
    {                                                                          \
      const integral::value_type result { value SYMBOL static_cast<integral::value_type>(rhs.as<decimal<32>>().value) }; \
      return make<decimal<32>>(result.convert_to<decimal<32>::value_type>());  \
    }                                                                          \
    else if (rhs.is<decimal<64>>())                                            \
    {                                                                          \
      const integral::value_type result { value SYMBOL static_cast<integral::value_type>(rhs.as<decimal<64>>().value) }; \
      return make<decimal<64>>(result.convert_to<decimal<64>::value_type>());  \
    }                                                                          \
    else if (rhs.is<integral>())                                                \
    {                                                                          \
      return make<integral>(value SYMBOL rhs.as<integral>().value);              \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      std::stringstream ss {};                                                 \
      ss << "no viable " OPERATION " with " << *this << " and " << rhs;        \
      throw std::logic_error { ss.str() };                                     \
    }                                                                          \
  } static_assert(true, "semicolon required after this macro")

  DEFINE_BINARY_ARITHMETIC_INTEGER(*, "multiplication");
  DEFINE_BINARY_ARITHMETIC_INTEGER(+, "addition");
  DEFINE_BINARY_ARITHMETIC_INTEGER(-, "subtraction");
  DEFINE_BINARY_ARITHMETIC_INTEGER(/, "division");
}} // namespace meevax::kernel

#undef DEFINE_BINARY_ARITHMETIC_INTEGER
#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
