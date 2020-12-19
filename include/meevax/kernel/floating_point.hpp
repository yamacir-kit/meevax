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

    auto as_exact() const
    {
      return static_cast<exact_integer>(value);
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
  auto operator <<(output_port & os, floating_point<T> const& rhs) -> output_port &
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
      return os << cyan << rhs.value << reset;
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
