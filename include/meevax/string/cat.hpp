#ifndef INCLUDED_MEEVAX_STRING_CAT_HPP
#define INCLUDED_MEEVAX_STRING_CAT_HPP

#include <sstream>

namespace meevax
{
  template <typename... Ts>
  auto cat(Ts&&... xs)
  {
    std::stringstream port {};
    (port << ... << xs);
    return port.str();
  }
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_CAT_HPP
