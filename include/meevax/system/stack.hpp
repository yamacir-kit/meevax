#ifndef INCLUDED_MEEVAX_SYSTEM_STACK_HPP
#define INCLUDED_MEEVAX_SYSTEM_STACK_HPP

#include <meevax/system/iterator.hpp>
#include <meevax/system/srfi-1.hpp>

namespace meevax::system
{
  /**
   * Stack structure provides Scheme-like stack operation to linear list.
   */
  struct stack
    : public iterator
  {
    template <typename... Ts>
    constexpr stack(Ts&&... xs)
      : iterator {std::forward<Ts>(xs)...}
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
    decltype(auto) emplace(Ts&&... xs)
    {
      return push(make<T>(std::forward<Ts>(xs)...));
    }

    template <typename... Ts>
    decltype(auto) pop(Ts&&... xs) // TODO specialize if in range 4 (standard cxr)
    {
      return std::advance(*this, std::forward<Ts>(xs)...);
    }

    decltype(auto) pop()
    {
      const auto buffer {top()};
      pop(1);
      return buffer;
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_STACK_HPP

