#ifndef INCLUDED_MEEVAX_CORE_NUMBER_HPP
#define INCLUDED_MEEVAX_CORE_NUMBER_HPP

#include <iostream>

#include <boost/multiprecision/gmp.hpp>

#include <meevax/core/cursor.hpp>

namespace meevax::core
{
  using number = boost::multiprecision::mpf_float;

  std::ostream& operator<<(std::ostream& os, const number& number)
  {
    return os << "\x1B[36m" << number.str() << "\x1B[0m";
  }

  #define DEFINE_NUMERIC_BINARY_OPERATOR(OPERATOR) \
  cursor operator OPERATOR(const cursor& lhs, const cursor& rhs) \
  { \
    return cursor::bind<number>( \
      lhs.data().as<number>() OPERATOR rhs.data().as<number>() \
    ); \
  }

  DEFINE_NUMERIC_BINARY_OPERATOR(+)
  DEFINE_NUMERIC_BINARY_OPERATOR(*)
  DEFINE_NUMERIC_BINARY_OPERATOR(-)
  DEFINE_NUMERIC_BINARY_OPERATOR(/)
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_NUMBER_HPP

