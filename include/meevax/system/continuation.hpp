#ifndef INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP
#define INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP

#include <meevax/system/object.hpp>
#include <meevax/system/writer.hpp>

namespace meevax::system
{
  DERIVE(continuation, public virtual, pair)

  std::ostream& operator<<(std::ostream& os, const continuation& continuation)
  {
    return os << color::syntax << "#("
              << color::constructor << "continuation"
              << color::normal << color::faint << " ;#" << &continuation
              << color::syntax << ")"
              << color::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP

