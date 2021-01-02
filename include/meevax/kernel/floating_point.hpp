#ifndef INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
#define INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP

#include <meevax/kernel/numerical_types.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T>
  struct floating_point
    : public std::numeric_limits<T>
  {
    using value_type = T;

    value_type value;

    explicit constexpr floating_point(T value = {})
      : value { value }
    {}

    template <typename... Ts>
    explicit constexpr floating_point(Ts&&... xs)
      : value { boost::lexical_cast<value_type>(std::forward<decltype(xs)>(xs)...) }
    {}

    constexpr auto is_integer() const noexcept
    {
      return value == std::trunc(value);
    }

    auto to_string() const
    {
      return boost::lexical_cast<std::string>(value);
    }

    auto as_exact() const -> decltype(auto)
    {
      /* ---- R7RS 6.2.6 (exact z) ---------------------------------------------
       *
       *  The procedure exact returns an exact representation of z. The value
       *  returned is the exact number that is numerically closest to the
       *  argument. For exact arguments, the result is the same as the argument.
       *  For inexact non-integral real arguments, the implementation may return
       *  a rational approximation, or may report an implementation
       *
       * -------------------------------------------------------------------- */

      return ratio(value);
    }

    template <typename U, REQUIRES(std::is_floating_point<U>)>
    constexpr auto as_inexact() const noexcept
    {
      return floating_point<U>(value);
    }

    constexpr operator value_type() const noexcept { return value; }
    constexpr operator value_type()       noexcept { return value; }
  };

  template <typename T>
  auto operator <<(output_port & port, floating_point<T> const& rhs) -> output_port &
  {
    if (std::isnan(rhs))
    {
      return port << cyan << "+nan.0" << reset;
    }
    else if (std::isinf(rhs))
    {
      return port << cyan << (0 < rhs.value ? '+' : '-') << "inf.0" << reset;
    }
    else
    {
      return port << cyan << rhs.value << (rhs.is_integer() ? ".0" : "") << reset;
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
