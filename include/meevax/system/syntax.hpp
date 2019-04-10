#ifndef INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

#include <functional> // std::function
#include <iostream> // std::ostream
#include <string> // std::string
#include <utility> // std::forward

#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  using syntax_signature = std::function<cursor (const cursor&, const cursor&, const cursor&)>;

  struct syntax
    : public syntax_signature
  {
    const std::string name;

    template <typename... Ts>
    syntax(const std::string& name, Ts&&... args)
      : syntax_signature {std::forward<Ts>(args)...}
      , name {name}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const syntax& syntax)
  {
    return os << "#<syntax " << syntax.name << ">";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

