#include <meevax/system/accessor.hpp>
#include <meevax/system/boolean.hpp>
#include <meevax/system/character.hpp>
#include <meevax/system/closure.hpp>
#include <meevax/system/enclosure.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/pair.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/special.hpp>
#include <meevax/system/string.hpp>

namespace meevax::system
{
  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << "\x1b[0;36m#\\" << static_cast<const std::basic_string<char8_t>&>(c) << "\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const closure& closure)
  {
    return os << "\x1b[0;36m#<closure " << &closure << ">\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const exception& e)
  {
    return os << "\x1b[31m#<exception \"" << e.what() << "\">\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const error& e)
  {
    return os << "\x1b[31m#<error \"" << e.what() << "\">\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const warning& w)
  {
    return os << "\x1b[33m#<warning \"" << w.what() << "\">\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const number& number)
  {
    return os << "\x1B[36m" << number.str() << "\x1B[0m";
  }

  std::ostream& operator<<(std::ostream& os, const pair& p)
  {
    os << "\x1b[35m(\x1b[0m" << std::get<0>(p);

    for (auto e {std::get<1>(p)}; e; e = cdr(e))
    {
      if (e.is<pair>())
      {
        os << " " << car(e);
      }
      else // iter is the last element of dotted-list.
      {
        os << "\x1b[35m . \x1b[0m" << e;
      }
    }

    return os << "\x1b[35m)\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const procedure& procedure)
  {
    return os << "\x1b[0;36m#<procedure " << procedure.name << ">\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const string& s)
  {
    os << "\x1b[36m\"" << std::get<0>(s).as<std::string>();

    for (const auto& each : std::get<1>(s))
    {
      os << each.as<std::string>();
    }

    return os << "\"\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const special& special)
  {
    return os << "\x1B[0;36m#<special " << special.name << ">\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const enclosure& closure)
  {
    return os << "\x1B[0;36m#<enclosure " << &closure << ">\x1b[0m";
  }
} // namespace meevax::system

