#ifndef INCLUDED_SCHEME_BASE_HPP
#define INCLUDED_SCHEME_BASE_HPP

#include <numeric>

#include <meevax/system/boolean.hpp>
#include <meevax/system/cursor.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/operator.hpp>

extern "C"
{
  using namespace meevax::system;

  cursor eq(const cursor&);
  cursor is_pair(const cursor&);

  cursor divide(const cursor&);
  cursor minus(const cursor&);
  cursor multiply(const cursor&);
  cursor plus(const cursor&);
} // extern "C"

#endif // INCLUDED_SCHEME_BASE_HPP

