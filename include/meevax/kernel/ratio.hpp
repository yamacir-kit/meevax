#ifndef INCLUDED_MEEVAX_KERNEL_RATIO_HPP
#define INCLUDED_MEEVAX_KERNEL_RATIO_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax { inline namespace kernel
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

    auto reduce() -> const ratio&;

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

    // auto operator * (const object&) const -> object;
    // auto operator + (const object&) const -> object;
    // auto operator - (const object&) const -> object;
    // auto operator / (const object&) const -> object;
    // auto operator % (const object&) const -> object;
  };

  auto operator <<(std::ostream& port, const ratio&) -> decltype(port);

  auto operator +(const ratio&, const ratio&) -> ratio;
  auto operator -(const ratio&, const ratio&) -> ratio;
  auto operator *(const ratio&, const ratio&) -> ratio;
  auto operator /(const ratio&, const ratio&) -> ratio;

  auto operator ==(const ratio&, const ratio&) -> bool;
  auto operator !=(const ratio&, const ratio&) -> bool;
  auto operator < (const ratio&, const ratio&) -> bool;
  auto operator <=(const ratio&, const ratio&) -> bool;
  auto operator > (const ratio&, const ratio&) -> bool;
  auto operator >=(const ratio&, const ratio&) -> bool;
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
