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

namespace meevax::kernel
{
  struct compilation_context // TODO rename
  {
    bool tail_expression;
    bool program_declaration;
  };

  static constexpr compilation_context as_is {
    false, false
  };

  static constexpr compilation_context as_tail_expression {
    true, false
  };

  static constexpr compilation_context as_program_declaration {
    false, true
  };

  static constexpr compilation_context as_tail_expression_of_program_declaration {
    true, true
  };

  struct syntax
    : public std::function<SYNTAX()>
  {
    using signature = SYNTAX((*));

    const std::string name;

    template <typename... Ts>
    syntax(const std::string& name, Ts&&... operands)
      : std::function<SYNTAX()> {
          std::forward<decltype(operands)>(operands)...
        }
      , name {name}
    {}

    friend auto operator<<(std::ostream& os, const syntax& syntax)
      -> decltype(auto)
    {
      return os << posix::highlight::syntax  << "#,("
                << posix::highlight::type    << "syntax"
                << posix::attribute::normal  << " " << syntax.name
                << posix::highlight::comment << " #;" << &syntax
                << posix::attribute::normal
                << posix::highlight::syntax  << ")"
                << posix::attribute::normal;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP

