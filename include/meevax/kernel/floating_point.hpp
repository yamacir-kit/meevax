#ifndef INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
#define INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP

#include <limits>
#include <valarray>

#include <meevax/kernel/numeric_tower.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T, REQUIRES(std::is_floating_point<T>)>
  auto rationalize(T x, T const e = std::numeric_limits<double>::epsilon())
  {
    int sign  = x > 0 ? 1 : -1;

    x = std::abs(x);

    std::valarray<T> v1 { static_cast<T>(static_cast<int>(x)), 1 },
                     v2 { 1, 0 };

    /* ---- Continued Fraction Expantion -------------------------------------
     *
     *                        1
     *  x_0 = a_0 + ---------------------
     *                           1
     *              a_1 + ---------------
     *                              1
     *                    a_2 + ---------
     *                                 1
     *                          a_n + ---
     *                                 e
     *
     * -------------------------------------------------------------------- */
    auto x_n = x - static_cast<int>(x);

    while (e < x_n)
    {
      auto a_n = 1 / x_n;

      x_n = a_n - static_cast<int>(a_n);

      auto old_1 = v1;
      v1 = static_cast<T>(static_cast<int>(a_n)) * v1 + v2;
      v2 = old_1;
    }

    return ratio(make<exact_integer>(sign * v1[0]),
                 make<exact_integer>(       v1[1]));
  }

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
      return boost::lexical_cast<bytestring>(value);
    }

    template <typename... Ts>
    auto as_exact(Ts&&... xs) const -> decltype(auto)
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

      return rationalize(value, std::forward<decltype(xs)>(xs)...);
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
      return port << cyan << std::fixed << rhs.value << reset;
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_FLOATING_POINT_HPP
