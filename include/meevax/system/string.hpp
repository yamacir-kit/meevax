#ifndef INCLUDED_MEEVAX_SYSTEM_STRING_HPP
#define INCLUDED_MEEVAX_SYSTEM_STRING_HPP

#include <iostream>
#include <string>

#include <meevax/system/character.hpp>
#include <meevax/system/cursor.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
{
  struct string
    : public virtual pair
  {};

  std::ostream& operator<<(std::ostream& os, const string& s)
  {
    os << "\x1b[36m\"" << s.first.as<std::string>();

    for (cursor c {s.second}; c; ++c)
    {
      os << (*c).as<std::string>();
    }

    return os << "\"\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_STRING_HPP

