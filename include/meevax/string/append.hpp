#ifndef INCLUDED_MEEVAX_STRING_APPEND_HPP
#define INCLUDED_MEEVAX_STRING_APPEND_HPP

#include <sstream>

namespace meevax
{
  template <typename... Ts>
  auto string_append(Ts&&... xs)
  {
    std::stringstream port {};
    (port << ... << xs);
    return port.str();
  }
} // namespace meevax

#endif // INCLUDED_MEEVAX_STRING_APPEND_HPP
