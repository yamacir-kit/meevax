#ifndef INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

#include <meevax/system/closure.hpp>

namespace meevax::system
{
  struct syntax
    : public closure
  {};

  std::ostream& operator<<(std::ostream&, const syntax&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

