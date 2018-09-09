#ifndef INCLUDED_MEEVAX_LISP_FUNCTION_HPP
#define INCLUDED_MEEVAX_LISP_FUNCTION_HPP

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
    if (lhs == rhs)
    {
      return true;
    }
    else if (lhs->type() == typeid(T) && rhs->type() == typeid(T))
    {
      return lhs->as<T>() == rhs->as<T>();
    }
    else
    {
      return false;
    }
  }

  template <typename T, typename U>
  auto list(T&& lhs, U&& rhs)
    -> decltype(auto)
  {
    return cons(
             std::forward<T>(lhs),
             cons(
               std::forward<U>(rhs),
               cell::nil
             )
           );
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
      return cell::nil;
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

