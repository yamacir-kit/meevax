#ifndef INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP
#define INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP

#include <iostream>

#include <meevax/system/pair.hpp>
#include <meevax/utility/perfect_derive.hpp>

namespace meevax::system
{
  PERFECT_DERIVE(continuation, public virtual, pair)

  std::ostream& operator<<(std::ostream& os, const continuation& continuation)
  {
    return os << "\x1b[35m" << "#("
              << "\x1b[36m" << "continuation"
              << "\x1b[0m " << &continuation
              << "\x1b[35m" << ")"
              << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP

