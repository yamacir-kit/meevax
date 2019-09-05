#ifndef INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  // closure is pair of expression and lexical-environment
  DERIVE(closure, public virtual, pair)

  std::ostream& operator<<(std::ostream& os, const closure& closure)
  {
    return os << "\x1b[35m" << "#("
              << "\x1b[32m" << "closure"
              << "\x1b[0m " << "#;" << &closure
              << "\x1b[35m" << ")"
              << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

