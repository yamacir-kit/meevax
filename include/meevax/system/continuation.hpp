#ifndef INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP
#define INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  DERIVE(continuation, public virtual, pair)

  std::ostream& operator<<(std::ostream& os, const continuation& continuation)
  {
    return os << color::syntax << "#("
              << color::constructor << "continuation"
              << attribute::normal << attribute::faint << " ;#" << &continuation
              << color::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP

