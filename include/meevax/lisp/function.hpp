#ifndef INCLUDED_MEEVAX_LISP_FUNCTION_HPP
#define INCLUDED_MEEVAX_LISP_FUNCTION_HPP

#include <memory>
#include <string>
#include <utility>

#include <meevax/lisp/accessor.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/table.hpp>

namespace meevax::lisp
{
  template <typename T, typename U>
  decltype(auto) cons(T&& lhs, U&& rhs)
  {
    return std::make_shared<cell>(std::forward<T>(lhs), std::forward<U>(rhs));
  }

  template <typename T, typename U>
  bool eq(T&& lhs, U&& rhs)
  {
    return lhs == rhs;
  }

  template <typename T>
  decltype(auto) null(T&& e)
  {
    return eq(std::forward<T>(e), symbols.intern("nil"));
  }

  decltype(auto) list()
  {
    return cell::nil;
  }

  template <typename T, typename... Ts>
  decltype(auto) list(T&& head, Ts&&... tail)
  {
    return cons(std::forward<T>(head), list(std::forward<Ts>(tail)...));
  }

  auto append(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
    -> const std::shared_ptr<cell>
  {
    return null(x)
             ? y
             : cons(
                 car(x),
                 append(cdr(x), y)
               );
  }

  auto zip(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
    -> const std::shared_ptr<cell>
  {
    if (null(x) && null(y))
    {
      return symbols.intern("nil");
    }
    else if (!atom(x) && !atom(y))
    {
      return cons(
               list(car(x), car(y)),
               zip(cdr(x), cdr(y))
             );
    }
    else
    {
      return symbols.intern("nil");
    }
  }

  auto assoc(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
    -> const std::shared_ptr<cell>
  {
    if (null(x))
    {
      return symbols.intern("nil");
    }
    else if (null(y))
    {
      return x;
    }
    else
    {
      return eq(caar(y), x) ? cadar(y) : assoc(x, cdr(y));
    }
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_FUNCTION_HPP

