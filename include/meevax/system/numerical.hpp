#ifndef INCLUDED_MEEVAX_SYSTEM_NUMERICAL_HPP
#define INCLUDED_MEEVAX_SYSTEM_NUMERICAL_HPP

#include <utility>

#include <boost/multiprecision/gmp.hpp>
#include <boost/multiprecision/mpfr.hpp>

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  using integral
    = boost::multiprecision::number<
        boost::multiprecision::gmp_int,
        boost::multiprecision::et_off
      >;

  std::ostream& operator<<(std::ostream& os, const integral& integral)
  {
    return os << "\x1B[36m" << integral.str() << "\x1B[0m";
  }

  // using real_base
  using real
    = boost::multiprecision::number<
        boost::multiprecision::mpfr_float_backend<0>,
        boost::multiprecision::et_off
      >;

  // struct real
  //   : public real_base
  // {
  //   visual::point position;
  //
  //   template <typename... Ts>
  //   explicit constexpr real(Ts&&... xs)
  //     : real_base {std::forward<Ts>(xs)...}
  //   {}
  //
  //   const auto& boost() const noexcept
  //   {
  //     return static_cast<const real_base&>(*this);
  //   }
  // };

  std::ostream& operator<<(std::ostream& os, const real& real)
  {
    return os << "\x1B[36m" << real.str() << "\x1B[0m";
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
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_NUMERICAL_HPP

