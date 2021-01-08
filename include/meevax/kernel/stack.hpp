#ifndef INCLUDED_MEEVAX_KERNEL_STACK_HPP
#define INCLUDED_MEEVAX_KERNEL_STACK_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T, typename... Ts>
  inline decltype(auto) push(T&& stack, Ts&&... xs)
  {
    const auto buffer {
      cons(
        std::forward<decltype(xs)>(xs)...,
        stack)
    };

    return stack = buffer;
  }

  template <std::size_t N, typename T>
  inline decltype(auto) pop(T&& stack)
  {
    return stack = std::next(begin(stack), N);
  }

  template <typename T>
  inline decltype(auto) pop(T&& stack)
  {
    const auto buffer {car(stack)};
    pop<1>(stack);
    return buffer;
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STACK_HPP
