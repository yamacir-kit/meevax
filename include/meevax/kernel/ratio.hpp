#ifndef INCLUDED_MEEVAX_KERNEL_RATIO_HPP
#define INCLUDED_MEEVAX_KERNEL_RATIO_HPP

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
      return numerator().as_inexact<T>() / denominator().as_inexact<T>();
    }
  };

  auto operator <<(output_port & port, ratio const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
