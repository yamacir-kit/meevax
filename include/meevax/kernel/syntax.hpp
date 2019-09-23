#ifndef INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP

#include <functional> // std::function

#include <meevax/kernel/object.hpp>

#define SYNTAX(NAME) object NAME(const object&, const object&, const object&, bool)

namespace meevax::kernel
{
  struct syntax
    : public std::function<SYNTAX()>
  {
    using signature = SYNTAX((*));

    const std::string name;

    template <typename... Ts>
    syntax(const std::string& name, Ts&&... operands)
      : std::function<SYNTAX()> {std::forward<decltype(operands)>(operands)...}
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
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP

