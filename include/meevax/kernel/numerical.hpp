#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <boost/multiprecision/gmp.hpp>
#include <boost/multiprecision/mpfr.hpp>

#include <meevax/kernel/pair.hpp>

namespace meevax::kernel
{
  /* ==== Numbers ==============================================================
   *
   *
   * ======================================================================== */

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
  };

  struct rational
    : public virtual pair
  {
  };

  struct integer
    : public boost::multiprecision::number<
               boost::multiprecision::gmp_int,
               boost::multiprecision::et_off>
  {
    using boost_integer
      = boost::multiprecision::number<
          boost::multiprecision::gmp_int,
          boost::multiprecision::et_off>;

    using boost_integer::boost_integer;

    // auto operator +(const object& rhs) const
    // {
    //   if (rhs.is<integer>())
    //   {
    //     // return make<integer>(*this + rhs.as<boost_integer>());
    //     return make<integer>(42);
    //   }
    //   else
    //   {
    //     return unit;
    //   }
    // }
  };

  std::ostream& operator<<(std::ostream& os, const integer& x)
  {
    return os << console::cyan << x.str() << console::reset;
  }

  // using real_base
  using real
    = boost::multiprecision::number<
        boost::multiprecision::mpfr_float_backend<0>,
        boost::multiprecision::et_off>;

  // struct real
  //   : public real_base
  // {
  //   visual::point position;
  //
  //   template <typename... Ts>
  //   explicit constexpr real(Ts&&... operands)
  //     : real_base {std::forward<decltype(operands)>(operands)...}
  //   {}
  //
  //   const auto& boost() const noexcept
  //   {
  //     return static_cast<const real_base&>(*this);
  //   }
  // };

  std::ostream& operator<<(std::ostream& os, const real& real)
  {
    return os << console::cyan << real.str() << console::reset;
  }

  #define DEFINE_NUMERICAL_BINARY_ARITHMETIC(OPERATOR)                         \
  decltype(auto) operator OPERATOR(const object& lhs, const object& rhs)       \
  {                                                                            \
    return make<real>(                                                         \
      lhs.as<const real>() OPERATOR rhs.as<const real>()                       \
    );                                                                         \
  }

  DEFINE_NUMERICAL_BINARY_ARITHMETIC(+)
  DEFINE_NUMERICAL_BINARY_ARITHMETIC(*)
  DEFINE_NUMERICAL_BINARY_ARITHMETIC(-)
  DEFINE_NUMERICAL_BINARY_ARITHMETIC(/)

  #define DEFINE_NUMERICAL_BINARY_COMPARISON(OPERATOR)                         \
  decltype(auto) operator OPERATOR(const object& lhs, const object& rhs)       \
  {                                                                            \
    return lhs.as<const real>() OPERATOR rhs.as<const real>();                 \
  }

  DEFINE_NUMERICAL_BINARY_COMPARISON(<)
  DEFINE_NUMERICAL_BINARY_COMPARISON(<=)
  DEFINE_NUMERICAL_BINARY_COMPARISON(>)
  DEFINE_NUMERICAL_BINARY_COMPARISON(>=)
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

