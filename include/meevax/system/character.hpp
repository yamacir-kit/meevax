#ifndef INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP
#define INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

#include <string>

#include <meevax/utility/perfect_derive.hpp>

namespace meevax::system
{
  PERFECT_DERIVE(character, public, std::string)

  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << "\x1b[0;36m#\\" << static_cast<const std::string&>(c) << "\x1b[0m";
  }

  constexpr auto backspace {u8'\b'};
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_CHARACTER_HPP

