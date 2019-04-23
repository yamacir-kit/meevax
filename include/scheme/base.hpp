#ifndef INCLUDED_LIBRARY_SCHEME_BASE_HPP
#define INCLUDED_LIBRARY_SCHEME_BASE_HPP

#include <meevax/system/cursor.hpp>
#include <meevax/system/instruction.hpp>
#include <meevax/system/operator.hpp>

extern "C"
{
  using namespace meevax::system;

  cursor quote(const cursor&, const cursor&, const cursor&);
} // extern "C"

#endif // INCLUDED_LIBRARY_SCHEME_BASE_HPP

