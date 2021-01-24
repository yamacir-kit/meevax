#ifndef INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP

#include <bitset>

#include <cstdint>
#include <meevax/kernel/object.hpp>

#define SYNTAX(NAME)                                                           \
  let const NAME(                                                              \
    [[maybe_unused]] syntactic_contexts const& the_expression_is,              \
    [[maybe_unused]] let const& expression,                                    \
    [[maybe_unused]] let const& syntactic_environment,                         \
    [[maybe_unused]] let const& frames,                                        \
    [[maybe_unused]] let const& continuation)

namespace meevax
{
inline namespace kernel
{
  struct syntactic_contexts
  {
    std::bitset<2> data;

    template <typename... Ts>
    explicit constexpr syntactic_contexts(Ts&&... xs)
      : data { std::forward<decltype(xs)>(xs)... }
    {}

    decltype(auto) at_the_top_level() const
    {
      return data.test(0);
    }

    decltype(auto) in_a_tail_context() const
    {
      return data.test(1);
    }

    decltype(auto) take_over(syntactic_contexts const& contexts)
    {
      data |= contexts.data;
      return *this;
    }

    auto take_over(syntactic_contexts const& contexts) const
    {
      syntactic_contexts result { data | contexts.data };
      return result;
    }
  };

  constexpr syntactic_contexts in_context_free {};

  constexpr syntactic_contexts at_the_top_level  { static_cast<std::uint64_t>(0b01) };
  constexpr syntactic_contexts in_a_tail_context { static_cast<std::uint64_t>(0b10) };

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
