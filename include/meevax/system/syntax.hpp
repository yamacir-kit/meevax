#ifndef INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP
#define INCLUDED_MEEVAX_SYSTEM_SYNTAX_HPP

#include <functional> // std::function

#include <meevax/system/closure.hpp>
#include <meevax/system/cursor.hpp>

namespace meevax::system
{
  struct native_syntax
    : public std::function<objective (const objective&, const objective&, const objective&)>
  {
    using signature = objective (*)(const objective&, const objective&, const objective&);

    const std::string name;

    template <typename... Ts>
    native_syntax(const std::string& name, Ts&&... args)
      : std::function<objective (const objective&, const objective&, const objective&)> {std::forward<Ts>(args)...}
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

