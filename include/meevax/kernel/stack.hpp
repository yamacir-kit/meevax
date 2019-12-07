#ifndef INCLUDED_MEEVAX_KERNEL_STACK_HPP
#define INCLUDED_MEEVAX_KERNEL_STACK_HPP

#include <meevax/kernel/list.hpp>

namespace meevax::kernel
{
  /* ==== Stack ===============================================================
  *
  * Stack structure provides Scheme-like stack operation to linear list.
  *
  *========================================================================== */
  [[deprecated]]
  struct stack
    : public homoiconic_iterator
  {
    template <typename... Ts>
    constexpr stack(Ts&&... operands)
      : homoiconic_iterator {std::forward<decltype(operands)>(operands)...}
    {}

    [[deprecated]]
    decltype(auto) top() const
    {
      return operator*();
    }

    [[deprecated]]
    decltype(auto) empty() const noexcept
    {
      return not *this;
    }

    [[deprecated]]
    decltype(auto) size() const
    {
      return length(*this);
    }

    template <typename... Objects>
    [[deprecated]]
    decltype(auto) push(Objects&&... objects)
    {
      return *this = cons(std::forward<Objects>(objects)..., *this);
    }

    template <typename T, typename... Ts>
    [[deprecated]]
    decltype(auto) emplace(Ts&&... operands)
    {
      return push(make<T>(std::forward<decltype(operands)>(operands)...));
    }

    [[deprecated]]
    void pop(std::size_t size)
    {
      switch (size)
      {
      case 1:
        *this = cdr(*this);
        break;

      case 2:
        *this = cddr(*this);
        break;

      case 3:
        *this = cdddr(*this);
        break;

      case 4:
        *this = cddddr(*this);
        break;

      default:
        std::advance(*this, size);
      }
    }

    [[deprecated]]
    decltype(auto) pop()
    {
      const auto buffer {top()};
      pop(1);
      return buffer;
    }
  };

  template <typename T, typename... Ts>
  decltype(auto) push(T&& place, Ts&&... operands)
  {
    const auto buffer {
      cons(
        std::forward<decltype(operands)>(operands)...,
        place)
    };

    return place = buffer;
  }

  template <auto N, typename T>
  decltype(auto) pop(T&& variable)
  {
    return variable = std::next(std::begin(variable), N);
  }

  template <typename T>
  decltype(auto) pop(T&& variable)
  {
    const auto buffer {car(variable)};
    pop<1>(variable);
    return buffer;
  }

} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_STACK_HPP

