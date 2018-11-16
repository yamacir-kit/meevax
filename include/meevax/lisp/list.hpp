#ifndef INCLUDED_MEEVAX_LISP_LIST_HPP
#define INCLUDED_MEEVAX_LISP_LIST_HPP

#include <iterator>
#include <utility>

#include <meevax/lisp/cell.hpp>

#define caar(e) car(car(e))
#define cadar(e) car(cdr(car(e)))
#define caddar(e) car(cdr(cdr(car(e))))

#define cadr(e) car(cdr(e))
#define caddr(e) car(cdr(cdr(e)))
#define cadddr(e) car(cdr(cdr(cdr(e))))

namespace meevax::lisp
{
  tuple::accessor<0> car {};
  tuple::accessor<1> cdr {};

  auto cons = [](auto&& head, auto&& tail) -> cursor
  {
    return std::make_shared<cell>(head, tail);
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

  decltype(auto) length(const cursor& exp)
  {
    return std::distance(exp, symbols("nil"));
  }

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
    return !var || !env ? symbols("nil")
                        : var == **env ? cadar(env)
                                       : lookup(var, cdr(env));
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_LIST_HPP

