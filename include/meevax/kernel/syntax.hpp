#ifndef INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP

#include <meevax/kernel/object.hpp>

#define SYNTAX(NAME)                                                           \
  const meevax::kernel::object NAME(                                           \
    const meevax::kernel::object&,                                             \
    const meevax::kernel::object&,                                             \
    const meevax::kernel::object&,                                             \
    const meevax::kernel::object&,                                             \
    const compilation_context)

namespace meevax { inline namespace kernel
{
  struct compilation_context // TODO rename
  {
    bool tail_expression;
    bool program_declaration;
  };

  constexpr compilation_context as_is { false, false };
  constexpr compilation_context as_tail_expression { true, false };
  constexpr compilation_context as_program_declaration { false, true };
  constexpr compilation_context as_tail_expression_of_program_declaration { true, true };

  struct syntax
    : public std::function<SYNTAX()>
  {
    using signature = SYNTAX((*));

    const std::string name;

    template <typename... Ts>
    syntax(const std::string& name, Ts&&... xs)
      : std::function<SYNTAX()> { std::forward<decltype(xs)>(xs)...  }
      , name {name}
    {}

    friend auto operator <<(std::ostream& os, const syntax& syntax) -> decltype(auto)
    {
      return os << magenta << "#,(" << green << "syntax" << reset << " " << syntax.name << faint << " #;" << &syntax << reset << magenta << ")" << reset;
    }
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
