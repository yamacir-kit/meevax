#ifndef INCLUDED_SCHEME_BASE_HPP
#define INCLUDED_SCHEME_BASE_HPP

#include <numeric>

#include <meevax/system/boolean.hpp>
#include <meevax/system/cursor.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/srfi-1.hpp>

extern "C"
{
  using namespace meevax::system;

  PROCEDURE(divides); // => division
  PROCEDURE(is_pair);
  PROCEDURE(minus); // => subtraction
  PROCEDURE(multiplies); // => multiplication
  PROCEDURE(plus); // => addition
  PROCEDURE(symbolic_equal);
} // extern "C"

#endif // INCLUDED_SCHEME_BASE_HPP

