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
  template <typename T>
  [[deprecated]] decltype(auto) null(T&& e)
  {
    return eq(std::forward<T>(e), symbols.intern("nil"));
  }

  [[deprecated]] decltype(auto) list()
  {
    return cell::nil;
  }

  template <typename T, typename... Ts>
  [[deprecated]] decltype(auto) list(T&& head, Ts&&... tail)
  {
    return cons(std::forward<T>(head), list(std::forward<Ts>(tail)...));
  }

  [[deprecated]] auto append(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
    -> const std::shared_ptr<cell>
  {
    return null(x)
             ? y
             : cons(
                 car(x),
                 append(cdr(x), y)
               );
  }

  [[deprecated]] auto zip(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
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

  [[deprecated]] auto assoc(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
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

