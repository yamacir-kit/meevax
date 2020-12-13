#ifndef INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
#define INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP

#include <type_traits>
#ifndef MEEVAX_USE_GMP
#define MEEVAX_USE_GMP
#endif

#ifdef MEEVAX_USE_GMP
#include <boost/multiprecision/gmp.hpp>
#else
#include <boost/multiprecision/cpp_int.hpp>
#endif

#include <meevax/kernel/algebra.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
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

    auto as_exact() const -> auto const&
    {
      return *this;
    }

    auto as_inexact() const
    {
      return value.convert_to<decltype(0.0)>();
    }

    operator value_type() const noexcept { return value; }
    operator value_type()       noexcept { return value; }
  };

  auto operator <<(output_port & port, exact_integer const&) -> output_port &;

  // let operator *(const exact_integer&, const object&);
  // let operator +(const exact_integer&, const object&);
  // let operator -(const exact_integer&, const object&);
  // let operator /(const exact_integer&, const object&);
  // let operator %(const exact_integer&, const object&);

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

  auto operator !=(exact_integer const&, exact_integer const&) -> bool;
  auto operator < (exact_integer const&, exact_integer const&) -> bool;
  auto operator <=(exact_integer const&, exact_integer const&) -> bool;
  auto operator ==(exact_integer const&, exact_integer const&) -> bool;
  auto operator > (exact_integer const&, exact_integer const&) -> bool;
  auto operator >=(exact_integer const&, exact_integer const&) -> bool;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
