#ifndef INCLUDED_MEEVAX_KERNEL_STACK_HPP
#define INCLUDED_MEEVAX_KERNEL_STACK_HPP

#include <meevax/kernel/list.hpp>

namespace meevax::kernel
{
  template <typename T, typename... Ts>
  inline decltype(auto) push(T&& variable, Ts&&... operands)
  {
    const auto buffer {
      cons(
        std::forward<decltype(operands)>(operands)...,
        variable)
    };

    return variable = buffer;
  }

  template <auto N, typename T>
  inline decltype(auto) pop(T&& variable)
  {
    return variable = std::next(begin(variable), N);
  }

  template <typename T>
  inline decltype(auto) pop(T&& variable)
  {
    const auto buffer {car(variable)};
    pop<1>(variable);
    return buffer;
  }

} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_STACK_HPP

