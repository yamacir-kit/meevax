#ifndef INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

#include <functional> // std::function

#include <meevax/system/object.hpp>

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
    return os << highlight::syntax << "#("
              << highlight::constructor << "syntax"
              << attribute::normal << " " << syntax.name
              << highlight::syntax << ")"
              << attribute::normal;
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

