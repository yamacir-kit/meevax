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
    return stack = cons(std::forward<decltype(xs)>(xs)..., stack);
  }

  template <std::size_t N, typename T>
  inline decltype(auto) pop(T&& stack)
  {
    return stack = std::next(std::begin(stack), N);
  }

  template <typename T>
  inline decltype(auto) pop(T&& stack)
  {
    let const x = car(stack);
    pop<1>(stack);
    return x;
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STACK_HPP
