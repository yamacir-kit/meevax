#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTEXT_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTEXT_HPP

#include <bitset>

namespace meevax
{
inline namespace kernel
{
  struct syntactic_context
  {
    std::bitset<2> data;

    template <typename... Ts>
    explicit constexpr syntactic_context(Ts&&... xs)
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

    decltype(auto) take_over(syntactic_context const& context)
    {
      data |= context.data;
      return *this;
    }

    auto take_over(syntactic_context const& context) const
    {
      syntactic_context result { data | context.data };
      return result;
    }
  };

  constexpr syntactic_context in_context_free   { static_cast<std::uint64_t>(0x00) };
  constexpr syntactic_context at_the_top_level  { static_cast<std::uint64_t>(0x01) };
  constexpr syntactic_context in_a_tail_context { static_cast<std::uint64_t>(0x02) };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTEXT_HPP
