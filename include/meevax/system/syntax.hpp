#ifndef INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

#include <functional> // std::function

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  struct syntax
    : public std::function<cursor (const cursor&, const cursor&, const cursor&)>
  {
    using signature = cursor (*)(const cursor&, const cursor&, const cursor&);

    const std::string name;

    template <typename... Ts>
    syntax(const std::string& name, Ts&&... args)
      : std::function<cursor (const cursor&, const cursor&, const cursor&)> {std::forward<Ts>(args)...}
      , name {name}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const syntax& syntax)
  {
    return os << "#<syntax " << syntax.name << ">";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

