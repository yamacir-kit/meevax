#ifndef INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

#include <functional> // std::function
#include <string>

#include <meevax/system/object.hpp>
#include <meevax/system/writer.hpp>

#define SYNTAX(NAME) object NAME(const object&, const object&, const object&, bool)

namespace meevax::system
{
  struct syntax
    : public std::function<SYNTAX()>
  {
    using signature = SYNTAX((*));

    const std::string name;

    template <typename... Ts>
    syntax(const std::string& name, Ts&&... args)
      : std::function<SYNTAX()> {std::forward<Ts>(args)...}
      , name {name}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const syntax& syntax)
  {
    return os << "\x1b[35m" << "#("
              << "\x1b[32m" << "syntax"
              << "\x1b[0m " << syntax.name
              << "\x1b[35m" << ")"
              << "\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

