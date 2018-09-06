#ifndef INCLUDED_MEEVAX_LISP_ACCESSOR_HPP
#define INCLUDED_MEEVAX_LISP_ACCESSOR_HPP

#include <memory>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  auto caar(const std::shared_ptr<cell>& e) noexcept
    -> decltype(auto)
  {
    return car(car(e));
  }

  auto cadr(const std::shared_ptr<cell>& e) noexcept
    -> decltype(auto)
  {
    return car(cdr(e));
  }

  auto cadar(const std::shared_ptr<cell>& e) noexcept
    -> decltype(auto)
  {
    return car(cdr(car(e)));
  }

  auto caddr(const std::shared_ptr<cell>& e) noexcept
    -> decltype(auto)
  {
    return car(cdr(cdr(e)));
  }

  auto caddar(const std::shared_ptr<cell>& e) noexcept
    -> decltype(auto)
  {
    return car(cdr(cdr(car(e))));
  }

  auto cadddr(const std::shared_ptr<cell>& e) noexcept
    -> decltype(auto)
  {
    return car(cdr(cdr(cdr(e))));
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_ACCESSOR_HPP


