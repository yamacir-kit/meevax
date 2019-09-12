#ifndef INCLUDED_MEEVAX_SYSTEM_STRING_HPP
#define INCLUDED_MEEVAX_SYSTEM_STRING_HPP

/*
 * This header is responsible for including <string>. But, this header knows
 * character.hpp includes <string>.
 */
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

      for (const auto& each : second)
      {
        buffer << each.as<std::string>();
      }

      return buffer.str();
    }
  };

  bool operator==(const string& lhs, const string& rhs)
  {
    return static_cast<std::string>(lhs) == static_cast<std::string>(rhs);
  }

  std::ostream& operator<<(std::ostream& os, const string& s)
  {
    os << highlight::simple_datum << "\"" << std::get<0>(s).as<std::string>();

    for (const auto& each : std::get<1>(s))
    {
      os << each.as<std::string>();
    }

    return os << "\"" << attribute::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_STRING_HPP

