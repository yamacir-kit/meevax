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

#include <meevax/kernel/numeric_tower.hpp>

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

    static constexpr std::true_type is_integer {};

    template <typename T>
    auto to() const
    {
      return value.convert_to<T>();
    }

    auto to_string() const
    {
      return value.str();
    }

    auto as_exact() const noexcept -> decltype(auto)
    {
      return *this;
    }

    template <typename T, REQUIRES(std::is_floating_point<T>)>
    auto as_inexact() const
    {
      return floating_point(to<T>());
    }

    operator value_type() const noexcept { return value; }
    operator value_type()       noexcept { return value; }
  };

  template <typename T, REQUIRES(std::is_integral<T>)> auto operator ==(exact_integer const& a, T&& b) { return a.value == b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator !=(exact_integer const& a, T&& b) { return a.value != b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator < (exact_integer const& a, T&& b) { return a.value <  b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator <=(exact_integer const& a, T&& b) { return a.value <= b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator > (exact_integer const& a, T&& b) { return a.value >  b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator >=(exact_integer const& a, T&& b) { return a.value >= b; }

  auto operator <<(output_port & port, exact_integer const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
