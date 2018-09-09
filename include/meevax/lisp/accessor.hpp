#ifndef INCLUDED_MEEVAX_LISP_ACCESSOR_HPP
#define INCLUDED_MEEVAX_LISP_ACCESSOR_HPP

#include <memory>
#include <utility>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  template <typename... Ts>
  constexpr auto caar(Ts&&... xs) noexcept
    -> decltype(auto)
  {
    return car(
             car(std::forward<Ts>(xs)...)
           );
  }

  template <typename... Ts>
  constexpr auto cadr(Ts&&... xs) noexcept
    -> decltype(auto)
  {
    return car(
             cdr(std::forward<Ts>(xs)...)
           );
  }

  template <typename... Ts>
  constexpr auto cadar(Ts&&... xs) noexcept
    -> decltype(auto)
  {
    return cadr(
             car(std::forward<Ts>(xs)...)
           );
  }

  template <typename... Ts>
  constexpr auto caddr(Ts&&... xs) noexcept
    -> decltype(auto)
  {
    return cadr(
             cdr(std::forward<Ts>(xs)...)
           );
  }

  template <typename... Ts>
  constexpr auto caddar(Ts&&... xs) noexcept
    -> decltype(auto)
  {
    return caddr(
             car(std::forward<Ts>(xs)...)
           );
  }

  template <typename... Ts>
  constexpr auto cadddr(Ts&&... xs) noexcept
    -> decltype(auto)
  {
    return caddr(
             cdr(std::forward<Ts>(xs)...)
           );
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_ACCESSOR_HPP


