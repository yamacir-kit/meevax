#ifndef INCLUDED_MEEVAX_SYSTEM_STRING_HPP
#define INCLUDED_MEEVAX_SYSTEM_STRING_HPP

#include <meevax/system/character.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
{
  struct string
    : public virtual pair
  {
    // TODO REPLACE TO BOOST::LEXICAL_CAST
    operator std::string() const
    {
      std::stringstream buffer {};
      buffer << first.as<std::string>();

      for (auto each : second)
      {
        buffer << each.as<std::string>();
      }

      return buffer.str();
    }
  };

  // TODO PROVIDE API "SYNTAX-HIGHLIGHT-OFF"
  std::ostream& operator<<(std::ostream& os, const string& s)
  {
    os << "\x1b[36m\"" << s.first.as<std::string>();

    for (auto each : s.second)
    {
      os << each.as<std::string>();
    }

    return os << "\"\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_STRING_HPP

