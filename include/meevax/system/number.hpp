#ifndef INCLUDED_MEEVAX_SYSTEM_NUMBER_HPP
#define INCLUDED_MEEVAX_SYSTEM_NUMBER_HPP

#include <boost/multiprecision/gmp.hpp>

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  using number = boost::multiprecision::mpf_float;

  std::ostream& operator<<(std::ostream& os, const number& number)
  {
    return os << "\x1B[36m" << number.str() << "\x1B[0m";
  }

  #define DEFINE_NUMERIC_BINARY_OPERATOR(OPERATOR) \
  decltype(auto) operator OPERATOR(const objective& lhs, const objective& rhs) \
  { \
    return make<number>( \
      lhs.as<number>() OPERATOR rhs.as<number>() \
    ); \
  }

  DEFINE_NUMERIC_BINARY_OPERATOR(+)
  DEFINE_NUMERIC_BINARY_OPERATOR(*)
  DEFINE_NUMERIC_BINARY_OPERATOR(-)
  DEFINE_NUMERIC_BINARY_OPERATOR(/)
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_NUMBER_HPP

