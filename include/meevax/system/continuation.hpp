#ifndef INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP
#define INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP

#include <meevax/system/pair.hpp>

namespace meevax::system
{
  DERIVE(continuation, public virtual, pair)

  std::ostream& operator<<(std::ostream& os, const continuation& continuation)
  {
    return os << highlight::syntax << "#("
              << highlight::constructor << "continuation"
              << attribute::normal << highlight::comment << " ;#" << &continuation
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CONTINUATION_HPP

