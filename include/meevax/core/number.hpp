#ifndef INCLUDED_MEEVAX_CORE_NUMBER_HPP
#define INCLUDED_MEEVAX_CORE_NUMBER_HPP

#include <iostream>

#include <boost/multiprecision/gmp.hpp>

namespace meevax::core
{
  using number = boost::multiprecision::mpf_float;

  std::ostream& operator<<(std::ostream& os, const number& number)
  {
    return os << "\x1B[36m" << number.str() << "\x1B[0m";
  }
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_NUMBER_HPP

