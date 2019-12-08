#ifndef INCLUDED_MEEVAX_KERNEL_STACK_HPP
#define INCLUDED_MEEVAX_KERNEL_STACK_HPP

#include <meevax/kernel/list.hpp>

namespace meevax::kernel
{
  template <typename T, typename... Ts>
  inline decltype(auto) push(T&& stack, Ts&&... operands)
  {
    const auto buffer {
      cons(
        std::forward<decltype(operands)>(operands)...,
        stack)
    };

    return stack = buffer;
  }

  template <auto N, typename T>
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
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_STACK_HPP

