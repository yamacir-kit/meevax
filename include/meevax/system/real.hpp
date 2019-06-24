#ifndef INCLUDED_MEEVAX_SYSTEM_REAL_HPP
#define INCLUDED_MEEVAX_SYSTEM_REAL_HPP

#include <boost/multiprecision/gmp.hpp>

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  using real = boost::multiprecision::number<
                 boost::multiprecision::gmp_float<0>,
                 boost::multiprecision::et_off
               >;

  std::ostream& operator<<(std::ostream& os, const real& real)
  {
    return os << "\x1B[36m" << real.str() << "\x1B[0m";
  }

  // TODO CHECK IF LHS OR RHS IS UNIQUE, THEN REWRITE VALUE ELSE MAKE NEW OBJECT
  #define DEFINE_NUMERIC_BINARY_OPERATOR(OPERATOR) \
  decltype(auto) operator OPERATOR(const object& lhs, const object& rhs) \
  { \
    return make<real>( \
      lhs.as<const real>() OPERATOR rhs.as<const real>() \
    ); \
  }

  DEFINE_NUMERIC_BINARY_OPERATOR(+)
  DEFINE_NUMERIC_BINARY_OPERATOR(*)
  DEFINE_NUMERIC_BINARY_OPERATOR(-)
  DEFINE_NUMERIC_BINARY_OPERATOR(/)
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_REAL_HPP

