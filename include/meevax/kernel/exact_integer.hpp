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
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_EXACT_INTEGER_HPP
