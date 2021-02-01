#ifndef INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP

#include <meevax/kernel/object.hpp>
#include <meevax/kernel/syntactic_context.hpp>

#define SYNTAX(NAME)                                                           \
  let const NAME(                                                              \
    [[maybe_unused]] syntactic_context const& the_expression_is,               \
    [[maybe_unused]] let      & syntactic_environment,                         \
    [[maybe_unused]] let const& expression,                                    \
    [[maybe_unused]] let const& frames,                                        \
    [[maybe_unused]] let const& continuation)

namespace meevax
{
inline namespace kernel
{
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
  };

  auto operator <<(output_port &, syntax const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
