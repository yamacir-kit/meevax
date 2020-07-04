#ifndef INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP

#include <boost/multiprecision/gmp.hpp>
#include <boost/multiprecision/mpfr.hpp>

#include <meevax/kernel/pair.hpp>

namespace meevax::kernel
{
  namespace multiprecision
  {
    using namespace boost::multiprecision;

    using real = number<mpfr_float_backend<0>, et_off>;
    // using real = number<gmp_float, et_off>;

    using integer = mpz_int;
  }

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

  struct real
    : public multiprecision::real
  {
    using multiprecision::real::real;

    auto operator +(const object& rhs) const
    {
      if (!rhs)
      {
        throw std::logic_error { "no viable addition with real and unit." };
      }
      else if (rhs.is<real>())
      {
        return
          make<real>(
            static_cast<const multiprecision::real&>(*this) + rhs.as<multiprecision::real>());
      }
      else
      {
        throw std::logic_error { "no viable addition with real and unknown." };
      }
    }

    friend std::ostream& operator<<(std::ostream& os, const real& x)
    {
      return os << console::cyan << x.str() << console::reset;
    }
  };

  auto operator ==(const real& x, const real& y)
  {
    return static_cast<const multiprecision::real&>(x)
        == static_cast<const multiprecision::real&>(y);
  }

  struct rational
    : public virtual pair
  {
  };

  struct integer
    : public multiprecision::integer
  {
    using multiprecision::integer::integer;

    auto operator +(const object& rhs) const
    {
      if (!rhs)
      {
        throw std::logic_error { "no viable addition with integer and unit." };
      }
      else if (rhs.is<integer>())
      {
        return
          make<integer>(
            static_cast<const multiprecision::integer&>(*this)
            + rhs.as<multiprecision::integer>());
      }
      else
      {
        throw std::logic_error { "" };
      }
    }

    friend std::ostream& operator<<(std::ostream& os, const integer& x)
    {
      return os << console::cyan << x.str() << console::reset;
    }
  };

  #define DEFINE_NUMERICAL_BINARY_ARITHMETIC(OPERATOR)                         \
  decltype(auto) operator OPERATOR(const object& lhs, const object& rhs)       \
  {                                                                            \
    return make<real>(                                                         \
      lhs.as<const multiprecision::real>() OPERATOR rhs.as<const multiprecision::real>() \
    );                                                                         \
  }

  // DEFINE_NUMERICAL_BINARY_ARITHMETIC(+)
  DEFINE_NUMERICAL_BINARY_ARITHMETIC(*)
  DEFINE_NUMERICAL_BINARY_ARITHMETIC(-)
  DEFINE_NUMERICAL_BINARY_ARITHMETIC(/)

  #define DEFINE_NUMERICAL_BINARY_COMPARISON(OPERATOR)                         \
  decltype(auto) operator OPERATOR(const object& lhs, const object& rhs)       \
  {                                                                            \
    return lhs.as<const multiprecision::real>() OPERATOR rhs.as<const multiprecision::real>(); \
  }

  DEFINE_NUMERICAL_BINARY_COMPARISON(<)
  DEFINE_NUMERICAL_BINARY_COMPARISON(<=)
  DEFINE_NUMERICAL_BINARY_COMPARISON(>)
  DEFINE_NUMERICAL_BINARY_COMPARISON(>=)
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_NUMERICAL_HPP
