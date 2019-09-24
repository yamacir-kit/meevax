#ifndef INCLUDED_MEEVAX_KERNEL_STACK_HPP
#define INCLUDED_MEEVAX_KERNEL_STACK_HPP

#include <meevax/kernel/list.hpp>

namespace meevax::kernel
{
  /**
   * Stack structure provides Scheme-like stack operation to linear list.
   */
  struct stack
    : public iterator
  {
    template <typename... Ts>
    constexpr stack(Ts&&... operands)
      : iterator {std::forward<decltype(operands)>(operands)...}
    {}

    decltype(auto) top() const
    {
      return operator*();
    }

    decltype(auto) empty() const noexcept
    {
      return not *this;
    }

    decltype(auto) size() const
    {
      return length(*this);
    }

    template <typename... Objects>
    decltype(auto) push(Objects&&... objects)
    {
      return *this = cons(std::forward<Objects>(objects)..., *this);
    }

    template <typename T, typename... Ts>
    decltype(auto) emplace(Ts&&... operands)
    {
      return push(make<T>(std::forward<decltype(operands)>(operands)...));
    }

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

    decltype(auto) pop()
    {
      const auto buffer {top()};
      pop(1);
      return buffer;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_STACK_HPP

