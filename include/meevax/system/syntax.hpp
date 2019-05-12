#ifndef INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

#include <functional> // std::function

#include <meevax/system/closure.hpp>
#include <meevax/system/cursor.hpp>

#define SYNTAX(NAME) objective NAME(const objective&, const objective&, const objective&)

namespace meevax::system
{
  struct native_syntax
    : public std::function<SYNTAX()>
  {
    using signature = SYNTAX((*));

    const std::string name;

    template <typename... Ts>
    native_syntax(const std::string& name, Ts&&... args)
      : std::function<SYNTAX()> {std::forward<Ts>(args)...}
      , name {name}
    {}
  };

  std::ostream& operator<<(std::ostream& os, const native_syntax& syntax)
  {
    return os << "#<native-syntax " << syntax.name << ">";
  }

  struct syntax
    : public closure
  {};

  std::ostream& operator<<(std::ostream& os, const syntax&)
  {
    return os << "#<syntax>";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

