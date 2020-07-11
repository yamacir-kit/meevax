#ifndef INCLUDED_MEEVAX_KERNEL_STACK_HPP
#define INCLUDED_MEEVAX_KERNEL_STACK_HPP

#include <meevax/kernel/list.hpp>

namespace meevax::kernel
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

  #if __cpp_nontype_template_parameter_auto
  template <auto N, typename T>
  #else
  template <std::size_t N, typename T>
  #endif
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

