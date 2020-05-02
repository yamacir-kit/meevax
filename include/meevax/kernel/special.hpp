#ifndef INCLUDED_MEEVAX_KERNEL_SPECIAL_HPP
#define INCLUDED_MEEVAX_KERNEL_SPECIAL_HPP

#include <meevax/kernel/object.hpp>

#define SPECIAL(NAME) \
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

  struct special
    : public std::function<SPECIAL()>
  {
    using signature = SPECIAL((*));

    const std::string name;

    template <typename... Ts>
    special(const std::string& name, Ts&&... operands)
      : std::function<SPECIAL()> {std::forward<decltype(operands)>(operands)...}
      , name {name}
    {}

    friend auto operator<<(std::ostream& os, const special& special)
      -> decltype(auto)
    {
      return os << posix::highlight::syntax  << "#,("
                << posix::highlight::type    << "special"
                << posix::attribute::normal  << " " << special.name
                << posix::highlight::comment << " #;" << &special
                << posix::attribute::normal
                << posix::highlight::syntax  << ")"
                << posix::attribute::normal;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SPECIAL_HPP

