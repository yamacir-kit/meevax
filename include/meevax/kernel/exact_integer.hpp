#ifndef INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
#define INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP

#ifndef MEEVAX_USE_GMP
#define MEEVAX_USE_GMP
#endif

#ifdef MEEVAX_USE_GMP
#include <boost/multiprecision/gmp.hpp>
#else
#include <boost/multiprecision/cpp_int.hpp>
#endif

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- Multi-Presition Exact Integer ----------------------------------------
   *
   * ------------------------------------------------------------------------ */
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

    template <typename T>
    auto is(T&& number) const
    {
      return value.convert_to<T>() == number;
    }

    auto as_exact() const -> const auto&
    {
      return *this;
    }

    operator value_type() const noexcept { return value; }
    operator value_type()       noexcept { return value; }
  };

  auto operator <<(std::ostream& port, const exact_integer&) -> decltype(port);

  let operator *(const exact_integer&, const object&);
  let operator +(const exact_integer&, const object&);
  let operator -(const exact_integer&, const object&);
  let operator /(const exact_integer&, const object&);
  let operator %(const exact_integer&, const object&);

  auto operator ==(const exact_integer&, const object&) -> bool;
  auto operator !=(const exact_integer&, const object&) -> bool;
  auto operator < (const exact_integer&, const object&) -> bool;
  auto operator <=(const exact_integer&, const object&) -> bool;
  auto operator > (const exact_integer&, const object&) -> bool;
  auto operator >=(const exact_integer&, const object&) -> bool;

  auto operator * (const exact_integer&, const exact_integer&) -> exact_integer;
  auto operator + (const exact_integer&, const exact_integer&) -> exact_integer;
  auto operator - (const exact_integer&, const exact_integer&) -> exact_integer;
  auto operator / (const exact_integer&, const exact_integer&) -> exact_integer;
  auto operator % (const exact_integer&, const exact_integer&) -> exact_integer;

  auto operator !=(const exact_integer&, const exact_integer&) -> bool;
  auto operator < (const exact_integer&, const exact_integer&) -> bool;
  auto operator <=(const exact_integer&, const exact_integer&) -> bool;
  auto operator ==(const exact_integer&, const exact_integer&) -> bool;
  auto operator > (const exact_integer&, const exact_integer&) -> bool;
  auto operator >=(const exact_integer&, const exact_integer&) -> bool;
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
