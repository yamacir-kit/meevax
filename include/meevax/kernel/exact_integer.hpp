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

#include <meevax/kernel/numerical_types.hpp>
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

    static constexpr std::true_type is_integer {};

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
      return floating_point(value.convert_to<T>());
    }

    operator value_type() const noexcept { return value; }
    operator value_type()       noexcept { return value; }
  };

  auto operator <<(output_port & port, exact_integer const&) -> output_port &;

  template <typename T, REQUIRES(std::is_integral<T>)> auto operator ==(exact_integer const& a, T&& b) { return a.value == b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator !=(exact_integer const& a, T&& b) { return a.value != b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator < (exact_integer const& a, T&& b) { return a.value <  b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator <=(exact_integer const& a, T&& b) { return a.value <= b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator > (exact_integer const& a, T&& b) { return a.value >  b; }
  template <typename T, REQUIRES(std::is_integral<T>)> auto operator >=(exact_integer const& a, T&& b) { return a.value >= b; }

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
  auto operator !=(exact_integer const&, exact_integer const&) -> bool;
  auto operator < (exact_integer const&, exact_integer const&) -> bool;
  auto operator <=(exact_integer const&, exact_integer const&) -> bool;
  auto operator ==(exact_integer const&, exact_integer const&) -> bool;
  auto operator > (exact_integer const&, exact_integer const&) -> bool;
  auto operator >=(exact_integer const&, exact_integer const&) -> bool;

  auto operator * (exact_integer const&, ratio const&) -> ratio;
  auto operator + (exact_integer const&, ratio const&) -> ratio;
  auto operator - (exact_integer const&, ratio const&) -> ratio;
  auto operator / (exact_integer const&, ratio const&) -> ratio;
  auto operator % (exact_integer const&, ratio const&) -> ratio;
  auto operator !=(exact_integer const&, ratio const&) -> bool;
  auto operator < (exact_integer const&, ratio const&) -> bool;
  auto operator <=(exact_integer const&, ratio const&) -> bool;
  auto operator ==(exact_integer const&, ratio const&) -> bool;
  auto operator > (exact_integer const&, ratio const&) -> bool;
  auto operator >=(exact_integer const&, ratio const&) -> bool;

  template <typename T> auto operator * (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() *  b; }
  template <typename T> auto operator + (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() +  b; }
  template <typename T> auto operator - (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() -  b; }
  template <typename T> auto operator / (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() /  b; }
  template <typename T> auto operator % (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() %  b; }
  template <typename T> auto operator !=(exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() != b; }
  template <typename T> auto operator < (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() <  b; }
  template <typename T> auto operator <=(exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() <= b; }
  template <typename T> auto operator ==(exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() == b; }
  template <typename T> auto operator > (exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() >  b; }
  template <typename T> auto operator >=(exact_integer const& a, floating_point<T> const& b) { return a.as_inexact<T>() >= b; }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
