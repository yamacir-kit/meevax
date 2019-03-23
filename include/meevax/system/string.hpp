#ifndef INCLUDED_MEEVAX_SYSTEM_STRING_HPP
#define INCLUDED_MEEVAX_SYSTEM_STRING_HPP

#include <iostream>
#include <string>

#include <meevax/system/cursor.hpp>
#include <meevax/system/pair.hpp>

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
    return os << "\x1B[31m#\\" << static_cast<std::string>(c) << "\x1b[0m";
  }

  struct string
    : public virtual pair
  {};

  std::ostream& operator<<(std::ostream& os, const string& s)
  {
    os << "\x1B[33m\"" << s.first.as<std::string>();

    for (cursor c {s.second}; c; c = cdr(c))
    {
      os << car(c).as<std::string>();
    }

    return os << "\"\x1B[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_STRING_HPP

