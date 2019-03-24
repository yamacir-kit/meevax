#ifndef INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP
#define INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

#include <iostream>
#include <string>
#include <utility>

namespace meevax::system
{
  struct character
    : public std::string
  {
    template <typename... Ts>
    constexpr character(Ts&&... args)
      : std::string {std::forward<Ts>(args)...}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << "\x1B[1;33m#\\" << static_cast<std::string>(c) << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

