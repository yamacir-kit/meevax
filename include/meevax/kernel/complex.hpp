#ifndef INCLUDED_MEEVAX_KERNEL_COMPLEX_HPP
#define INCLUDED_MEEVAX_KERNEL_COMPLEX_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct complex
    : public virtual pair
  {
    auto real() const noexcept -> decltype(auto) { return car(*this); }
    auto real()       noexcept -> decltype(auto) { return car(*this); }

    auto imag() const noexcept -> decltype(auto) { return cdr(*this); }
    auto imag()       noexcept -> decltype(auto) { return cdr(*this); }

    // friend auto operator +(const complex& lhs, const complex& rhs)
    // {
    //   return
    //     make<complex>(
    //       lhs.real() + rhs.real(),
    //       lhs.imag() + rhs.imag());
    // }
    //
    // template <typename T>
    // friend auto operator +(const complex& lhs, T&& rhs)
    // {
    //   return
    //     make<complex>(
    //       lhs.real() + rhs,
    //       lhs.imag());
    // }

    friend std::ostream& operator<<(std::ostream& os, const complex& z)
    {
      return os << cyan << z.real() << (0 < z.imag() ? '+' : '-') << z.imag() << "i" << reset;
    }
  };

} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_COMPLEX_HPP
