#ifndef INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  // closure is pair of expression and lexical-environment
  DERIVE(closure, public virtual, pair)

  std::ostream& operator<<(std::ostream& os, const closure& closure)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "closure"
              << highlight::comment << " #;" << &closure
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

