#ifndef INCLUDED_MEEVAX_KERNEL_RATIO_HPP
#define INCLUDED_MEEVAX_KERNEL_RATIO_HPP

#include <meevax/kernel/algebra.hpp>
#include <meevax/kernel/port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct ratio
    : public virtual pair
  {
    using pair::pair;

    auto numerator() const noexcept -> decltype(auto) { return car(*this); }
    auto numerator()       noexcept -> decltype(auto) { return car(*this); }

    auto denominator() const noexcept -> decltype(auto) { return cdr(*this); }
    auto denominator()       noexcept -> decltype(auto) { return cdr(*this); }

    auto is_integer() const -> bool;

    auto invert() const
    {
      return ratio(denominator(), numerator());
    }

    auto reduce() -> ratio const&;

    auto reduce() const
    {
      auto copy { *this };
      copy.reduce();
      return copy;
    }

    auto as_exact() const -> const auto&
    {
      return *this;
    }
  };

  auto operator <<(output_port & port, ratio const&) -> output_port &;

  auto operator !=(ratio const&, exact_integer const&) -> bool;
  auto operator < (ratio const&, exact_integer const&) -> bool;
  auto operator <=(ratio const&, exact_integer const&) -> bool;
  auto operator ==(ratio const&, exact_integer const&) -> bool;
  auto operator > (ratio const&, exact_integer const&) -> bool;
  auto operator >=(ratio const&, exact_integer const&) -> bool;

  auto operator * (ratio const&, ratio const&) -> ratio;
  auto operator + (ratio const&, ratio const&) -> ratio;
  auto operator - (ratio const&, ratio const&) -> ratio;
  auto operator / (ratio const&, ratio const&) -> ratio;
  auto operator % (ratio const&, ratio const&) -> ratio;
  auto operator ==(ratio const&, ratio const&) -> bool;
  auto operator !=(ratio const&, ratio const&) -> bool;
  auto operator < (ratio const&, ratio const&) -> bool;
  auto operator <=(ratio const&, ratio const&) -> bool;
  auto operator > (ratio const&, ratio const&) -> bool;
  auto operator >=(ratio const&, ratio const&) -> bool;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
