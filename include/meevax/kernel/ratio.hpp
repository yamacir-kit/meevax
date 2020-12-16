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

    auto numerator()   const noexcept -> decltype(auto) { return car(*this); }
    auto numerator()         noexcept -> decltype(auto) { return car(*this); }
    auto denominator() const noexcept -> decltype(auto) { return cdr(*this); }
    auto denominator()       noexcept -> decltype(auto) { return cdr(*this); }

    auto is_integer() const -> bool;

    auto invert() const
    {
      return ratio(denominator(), numerator());
    }

    auto reduce() const -> ratio;

    [[deprecated]]
    auto reduce() -> ratio const&;

    auto as_exact() const noexcept -> decltype(auto)
    {
      return *this;
    }

    template <typename T>
    auto as_inexact() const
    {
      return car(*this).as<exact_integer>().as_inexact<T>()
           / cdr(*this).as<exact_integer>().as_inexact<T>();
    }
  };

  auto operator <<(output_port & port, ratio const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
