#ifndef INCLUDED_MEEVAX_LISP_FUNCTION_HPP
#define INCLUDED_MEEVAX_LISP_FUNCTION_HPP

#include <iostream>
#include <memory>
#include <utility>

#include <meevax/lisp/accessor.hpp>
#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  template <typename T, typename U>
  auto cons(T&& lhs, U&& rhs)
    -> decltype(auto)
  {
    return std::make_shared<cell>(std::forward<T>(lhs), std::forward<U>(rhs));
  }

  template <typename T = std::string>
  bool eq(const std::shared_ptr<cell>& lhs, const std::shared_ptr<cell>& rhs)
  {
    return lhs == rhs || car(lhs)->as<T>() == car(rhs)->as<T>();
  }

  template <typename T>
  auto null(T&& e)
    -> decltype(auto)
  {
    return eq(std::forward<T>(e), cell::nil);
  }

  template <typename T, typename U>
  auto list(T&& lhs, U&& rhs)
    -> decltype(auto)
  {
    return cons(std::forward<T>(lhs), cons(std::forward<U>(rhs), cell::nil));
  }

  auto append(const std::shared_ptr<cell>& lhs, const std::shared_ptr<cell>& rhs)
    -> const std::shared_ptr<cell>
  {
    return null(lhs) ? rhs : cons(car(lhs), append(cdr(lhs), rhs));
  }

  auto pair(const std::shared_ptr<cell>& lhs, const std::shared_ptr<cell>& rhs)
    -> const std::shared_ptr<cell>
  {
    if (null(lhs) && null(rhs))
    {
      return cell::nil;
    }
    else if (!atom(lhs) && !atom(rhs))
    {
      return cons(
               list(car(lhs), car(rhs)),
               pair(cdr(lhs), cdr(rhs))
             );
    }
    else
    {
      return cell::nil;
    }
  }

  auto assoc(const std::shared_ptr<cell>& x, const std::shared_ptr<cell>& y)
    -> const std::shared_ptr<cell>
  {
    if (null(x))
    {
      return cell::nil;
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

