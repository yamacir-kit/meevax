#ifndef INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP

#include <meevax/kernel/object.hpp>

#define SYNTAX(NAME)                                                           \
  let const NAME(                                                              \
    [[maybe_unused]] syntactic_contexts const& the_expression_is,              \
    [[maybe_unused]] let const& expression,                                    \
    [[maybe_unused]] let const& syntactic_environment,                         \
    [[maybe_unused]] let const& frames,                                        \
    [[maybe_unused]] let const& continuation)

// expressions_is
//   .in_a_tail_context
//   .at_the_top_level

namespace meevax
{
inline namespace kernel
{
  struct syntactic_contexts
  {
    bool tail_expression;
    bool program_declaration;
  }
  constexpr in_context_free {};


  constexpr syntactic_contexts as_is { false, false };
  constexpr syntactic_contexts as_tail_expression { true, false };
  constexpr syntactic_contexts as_program_declaration { false, true };
  constexpr syntactic_contexts as_tail_expression_of_program_declaration { true, true };

  struct syntax
    : public std::function<SYNTAX()>
  {
    using signature = SYNTAX((*));

    bytestring const name;

    template <typename... Ts>
    explicit syntax(bytestring const& name, Ts&&... xs)
      : std::function<SYNTAX()> { std::forward<decltype(xs)>(xs)...  }
      , name { name }
    {}

    template <typename... Ts>
    decltype(auto) compile(Ts&&... xs)
    {
      return (*this)(std::forward<decltype(xs)>(xs)...);
    }

    friend auto operator <<(std::ostream& os, const syntax& syntax) -> decltype(auto)
    {
      return os << magenta << "#,(" << green << "syntax" << reset << " " << syntax.name << faint << " #;" << &syntax << reset << magenta << ")" << reset;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
