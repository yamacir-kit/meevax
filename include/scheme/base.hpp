#ifndef INCLUDED_SCHEME_BASE_HPP
#define INCLUDED_SCHEME_BASE_HPP

#include <numeric>

#include <meevax/system/boolean.hpp>
#include <meevax/system/cursor.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/operator.hpp>
#include <meevax/system/procedure.hpp>

extern "C"
{
  using namespace meevax::system;

  PROCEDURE(divides);
  PROCEDURE(is_pair);
  PROCEDURE(minus);
  PROCEDURE(multiplies);
  PROCEDURE(plus);
  PROCEDURE(symbolic_equal);
} // extern "C"

#endif // INCLUDED_SCHEME_BASE_HPP

