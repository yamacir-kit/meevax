#include <meevax/system/accessor.hpp>
#include <meevax/system/boolean.hpp>
#include <meevax/system/character.hpp>
#include <meevax/system/closure.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/module.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/pair.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/special.hpp>
#include <meevax/system/string.hpp>
#include <meevax/system/syntax.hpp>

namespace meevax::system
{
  template <typename T>
  std::ostream& operator<<(std::ostream& os, const accessor<T>& rhs)
  {
    // write(os) will be dispatched to each type's stream output operator.
    return !rhs ? (os << "\x1b[35m()\x1b[0m") : rhs.access().write(os);
  }

  std::ostream& operator<<(std::ostream& os, const character& c)
  {
    return os << "\x1B[0;36m#\\" << static_cast<const std::basic_string<char8_t>&>(c) << "\x1b[0m";
  }

  std::ostream& operator<<(std::ostream& os, const closure&)
  {
    return os << "#<closure>";
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

  std::ostream& operator<<(std::ostream& os, const module& module)
  {
    return os << "#<module " << module.name << ">";
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
    return os << "#<procedure " << procedure.name << ">";
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
    return os << "#<special " << special.name << ">";
  }

  std::ostream& operator<<(std::ostream& os, const syntax& syntax)
  {
    return os << "#<syntax " << &syntax << ">";
  }
} // namespace meevax::system

