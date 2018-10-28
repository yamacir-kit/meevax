#ifndef INCLUDED_MEEVAX_LISP_LIST_HPP
#define INCLUDED_MEEVAX_LISP_LIST_HPP

#include <utility>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  auto cons = [](auto&& head, auto&& tail) -> cursor
  {
    return std::make_shared<cell>(std::forward<decltype(head)>(head),
                                  std::forward<decltype(tail)>(tail));
  };

  template <typename T, typename U>
  decltype(auto) operator|(T&& head, U&& tail)
  {
    return cons(std::forward<T>(head), std::forward<U>(tail));
  }

  auto list = [](auto&&... args) constexpr
  {
    return (args | ... | symbols("nil"));
  };

  // TODO variadic
  cursor append(const cursor& x, const cursor& y)
  {
    return !x ? y : car(x) | append(cdr(x), y);
  }

  cursor zip(cursor x, cursor y)
  {
    if (!x && !y)
    {
      return symbols("nil");
    }
    else if (!atom(x) && !atom(y))
    {
      return list(car(x), car(y)) | zip(cdr(x), cdr(y));
    }
    else
    {
      return symbols("nil");
    }
  }

  cursor lookup(cursor var, cursor env)
  {
    return !var or !env ? symbols("nil") : var == **env ? cadar(env) : lookup(var, cdr(env));
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_LIST_HPP

