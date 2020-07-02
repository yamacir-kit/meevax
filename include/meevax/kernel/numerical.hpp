#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <boost/multiprecision/gmp.hpp>
#include <boost/multiprecision/mpfr.hpp>

#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  using integral
    = boost::multiprecision::number<
        boost::multiprecision::gmp_int,
        boost::multiprecision::et_off>;

  std::ostream& operator<<(std::ostream& os, const integral& integral)
  {
    return os << console::cyan << integral.str() << console::reset;
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
    return os << console::cyan << real.str()
              << console::reset;
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

