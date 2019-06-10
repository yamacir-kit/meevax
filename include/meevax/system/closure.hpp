#ifndef INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP
#define INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

#include <iostream>

#include <meevax/system/pair.hpp>
#include <meevax/utility/perfect_derive.hpp>

namespace meevax::system
{
  // closure is pair of expression and lexical-environment
  PERFECT_DERIVE(closure, public virtual, pair)

  std::ostream& operator<<(std::ostream& os, const closure& closure)
  {
    return os << "\x1b[0;36m#<closure " << &closure << ">\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CLOSURE_HPP

