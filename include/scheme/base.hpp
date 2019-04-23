#ifndef INCLUDED_SCHEME_BASE_HPP
#define INCLUDED_SCHEME_BASE_HPP

#include <numeric>

#include <meevax/system/boolean.hpp>
#include <meevax/system/cursor.hpp>
#include <meevax/system/instruction.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/operator.hpp>

extern "C"
{
  using namespace meevax::system;

  // cursor car(const cursor&, const cursor&, const cursor&);
  // cursor cdr(const cursor&, const cursor&, const cursor&);
  // cursor cons(const cursor&, const cursor&, const cursor&);
  // cursor define(const cursor&, const cursor&, const cursor&);
  // cursor if_(const cursor&, const cursor&, const cursor&);
  // cursor lambda(const cursor&, const cursor&, const cursor&);
  cursor quote(const cursor&, const cursor&, const cursor&);

  cursor eq(const cursor&);
  cursor is_pair(const cursor&);

  cursor plus(const cursor&);
  cursor multiply(const cursor&);
  cursor minus(const cursor&);
  cursor divide(const cursor&);
} // extern "C"

#endif // INCLUDED_SCHEME_BASE_HPP

