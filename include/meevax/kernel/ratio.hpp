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

    auto invert() const
    {
      return make<ratio>(denominator(), numerator());
    }

    auto reduce() -> const auto&;

    auto is_exact() const noexcept
    {
      return true;
    }

    auto is_inexact() const noexcept
    {
      return not is_exact();
    }

    // auto as_exact() const;

    auto as_inexact() const;

    // auto operator * (const object&) const -> object;
    // auto operator + (const object&) const -> object;
    // auto operator - (const object&) const -> object;
    // auto operator / (const object&) const -> object;
    // auto operator % (const object&) const -> object;
    //
    // auto operator ==(const object&) const -> bool;
    // auto operator !=(const object&) const -> bool;
    // auto operator < (const object&) const -> bool;
    // auto operator <=(const object&) const -> bool;
    // auto operator > (const object&) const -> bool;
    // auto operator >=(const object&) const -> bool;
  };

  auto operator <<(std::ostream& port, const ratio& rhs) -> decltype(auto)
  {
    return port << cyan << car(rhs)
                << cyan << "/"
                << cyan << cdr(rhs) << reset;
  }

  auto operator +(const ratio& lhs, const ratio& rhs)
  {
    // car(lhs)   car(rhs)   car(lhs) * cdr(rhs) + car(rhs) * cdr(lhs)
    // -------- + -------- = -----------------------------------------
    // cdr(lhs)   cdr(rhs)   cdr(lhs)            * cdr(rhs)

    return
      make<ratio>(
        car(lhs) * cdr(rhs) + car(rhs) * cdr(lhs),
        cdr(lhs)            * cdr(rhs));
  }

  auto operator -(const ratio& lhs, const ratio& rhs)
  {
    return
      make<ratio>(
        car(lhs) * cdr(rhs) - car(rhs) * cdr(lhs),
        cdr(lhs)            * cdr(rhs));
  }

  auto operator *(const ratio& lhs, const ratio& rhs)
  {
    return make<ratio>(car(lhs) * car(rhs), cdr(lhs) * cdr(rhs));
  }

  auto operator /(const ratio& lhs, const ratio& rhs)
  {
    return make<ratio>(car(lhs) * cdr(rhs), cdr(lhs) * car(rhs));
  }

  #define BOILERPLATE(SYMBOL)                                                  \
  auto operator SYMBOL(const ratio& lhs, const ratio& rhs)                     \
  {                                                                            \
    return (lhs.numerator() SYMBOL rhs.numerator())                            \
       and (lhs.denominator() SYMBOL rhs.denominator());                       \
  } static_assert(true)

  BOILERPLATE(==);
  BOILERPLATE(!=);
  BOILERPLATE(<);
  BOILERPLATE(<=);
  BOILERPLATE(>);
  BOILERPLATE(>=);

  #undef BOILERPLATE
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
