#ifndef INCLUDED_MEEVAX_KERNEL_RATIO_HPP
#define INCLUDED_MEEVAX_KERNEL_RATIO_HPP

#include <limits>
#include <valarray>

#include <meevax/kernel/numerical_types.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct ratio
    : public virtual pair
  {
    using pair::pair;

    template <typename T, REQUIRES(std::is_floating_point<T>)>
    auto rationalize(T x, T const e = std::numeric_limits<double>::epsilon())
    {
      int sign  = x > 0 ? 1 : -1;

      x = std::abs(x);

      std::valarray<T> v1 { static_cast<T>(static_cast<int>(x)), 1 },
                       v2 { 1, 0 };

      /* ---- Continued Fraction Expantion -------------------------------------
       *
       *                      1
       *  x = a_0 + ---------------------
       *                         1
       *            a_1 + ---------------
       *                            1
       *                  a_2 + ---------
       *                               1
       *                        a_n + ---
       *                               e
       *
       * -------------------------------------------------------------------- */
      auto x_n = x - static_cast<int>(x);

      while (e < x_n)
      {
        auto a_n = 1 / x_n;

        x_n = a_n - static_cast<int>(a_n);

        auto old_1 = v1;
        v1 = static_cast<int>(a_n) * v1 + v2;
        v2 = old_1;
      }

      return pair(make<exact_integer>(sign * v1[0]),
                  make<exact_integer>(       v1[1]));
    }

    template <typename... Ts>
    explicit ratio(Ts&&... xs)
      : pair { rationalize(std::forward<decltype(xs)>(xs)...) }
    {}

    auto numerator() const -> exact_integer const&;

    auto denominator() const -> exact_integer const&;

    auto is_integer() const -> bool;

    auto invert() const
    {
      return ratio(cdr(*this),
                   car(*this));
    }

    auto reduce() const -> ratio;

    auto as_exact() const noexcept -> ratio const&
    {
      return *this;
    }

    template <typename T>
    auto as_inexact() const
    {
      return floating_point(numerator().as_inexact<T>() / denominator().as_inexact<T>());
    }
  };

  auto operator <<(output_port & port, ratio const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
