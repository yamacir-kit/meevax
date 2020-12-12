#ifndef INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
#define INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP

#include <meevax/kernel/algebra.hpp>
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

    auto to_string() const
    {
      return boost::lexical_cast<std::string>(value);
    }

    constexpr auto is_integer() const noexcept
    {
      return value == std::trunc(value);
    }

    auto as_exact() const;

    constexpr operator value_type() const noexcept { return value; }
    constexpr operator value_type()       noexcept { return value; }
  };

  template <typename T> let operator * (floating_point<T>&, object const&);
  template <typename T> let operator + (floating_point<T>&, object const&);
  template <typename T> let operator - (floating_point<T>&, object const&);
  template <typename T> let operator / (floating_point<T>&, object const&);
  template <typename T> let operator % (floating_point<T>&, object const&);

  template <typename T> auto operator == (floating_point<T>&, const object&) -> bool;
  template <typename T> auto operator != (floating_point<T>&, const object&) -> bool;
  template <typename T> auto operator <  (floating_point<T>&, const object&) -> bool;
  template <typename T> auto operator <= (floating_point<T>&, const object&) -> bool;
  template <typename T> auto operator >  (floating_point<T>&, const object&) -> bool;
  template <typename T> auto operator >= (floating_point<T>&, const object&) -> bool;

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

  template <typename T, typename U> constexpr auto operator * (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value * b.value); }
  template <typename T, typename U> constexpr auto operator + (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value + b.value); }
  template <typename T, typename U> constexpr auto operator - (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value - b.value); }
  template <typename T, typename U> constexpr auto operator / (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(a.value / b.value); }
  template <typename T, typename U> constexpr auto operator % (floating_point<T> const& a, floating_point<U> const& b) { return floating_point(std::fmod(a.value, b.value)); }


  template <typename T, typename U> constexpr auto operator !=(floating_point<T> const& a, floating_point<U> const& b) { return a.value != b.value; }
  template <typename T, typename U> constexpr auto operator < (floating_point<T> const& a, floating_point<U> const& b) { return a.value <  b.value; }
  template <typename T, typename U> constexpr auto operator <=(floating_point<T> const& a, floating_point<U> const& b) { return a.value <= b.value; }
  template <typename T, typename U> constexpr auto operator ==(floating_point<T> const& a, floating_point<U> const& b) { return a.value == b.value; }
  template <typename T, typename U> constexpr auto operator > (floating_point<T> const& a, floating_point<U> const& b) { return a.value >  b.value; }
  template <typename T, typename U> constexpr auto operator >=(floating_point<T> const& a, floating_point<U> const& b) { return a.value >= b.value; }

  template <typename T> constexpr auto operator !=(floating_point<T> const& a, exact_integer const& b) { return a.value != b.as_inexact(); }
  template <typename T> constexpr auto operator < (floating_point<T> const& a, exact_integer const& b) { return a.value <  b.as_inexact(); }
  template <typename T> constexpr auto operator <=(floating_point<T> const& a, exact_integer const& b) { return a.value <= b.as_inexact(); }
  template <typename T> constexpr auto operator ==(floating_point<T> const& a, exact_integer const& b) { return a.value == b.as_inexact(); }
  template <typename T> constexpr auto operator > (floating_point<T> const& a, exact_integer const& b) { return a.value >  b.as_inexact(); }
  template <typename T> constexpr auto operator >=(floating_point<T> const& a, exact_integer const& b) { return a.value >= b.as_inexact(); }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
