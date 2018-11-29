#ifndef INCLUDED_MEEVAX_LISP_OPERATOR_HPP
#define INCLUDED_MEEVAX_LISP_OPERATOR_HPP

#include <iterator>
#include <typeindex>
#include <typeinfo>
#include <utility>

#include <meevax/lisp/pair.hpp>

#define caar(e) car(car(e))
#define cadr(e) car(cdr(e))

#define cadar(e) cadr(car(e))
#define caddr(e) cadr(cdr(e))

#define caddar(e) caddr(car(e))
#define cadddr(e) caddr(cdr(e))

namespace meevax::lisp
{
  template <typename... Ts>
  constexpr cursor cons(Ts&&... args)
  {
    return std::make_shared<pair>(std::forward<Ts>(args)...);
  }

  // For C++17 fold-expression
  template <typename T, typename U>
  constexpr decltype(auto) operator|(T&& car, U&& cdr)
  {
    return cons(std::forward<T>(car), std::forward<U>(cdr));
  }

  template <typename Cursor>
  bool atom(Cursor&& exp)
  {
    static const std::unordered_map<std::type_index, bool> dispatch
    {
      {typeid(pair), false},
      {typeid(closure), false},
      {typeid(std::string), true}
    };

    return !exp || dispatch.at(exp->type());
  }

  auto list = [](auto&&... args) constexpr
  {
    return (args | ... | nil);
  };

  // decltype(auto) length(const cursor& exp)
  // {
  //   return std::distance(exp, nil);
  // }

  template <typename T, typename U>
  cursor append(T&& x, U&& y)
  {
    return !x ? y : car(x) | append(cdr(x), y);
  }

  cursor zip(const cursor& x, const cursor& y)
  {
    if (!x && !y)
    {
      return nil;
    }
    else if (!atom(x) && !atom(y))
    {
      return list(car(x), car(y)) | zip(cdr(x), cdr(y));
    }
    else
    {
      return nil;
    }
  }

  const cursor& assoc(const cursor& var, const cursor& env)
  {
    if (!var || !env)
    {
      return nil;
    }
    else if (caar(env) == var)
    {
      return cadar(env);
    }
    else
    {
      return assoc(var, cdr(env));
    }
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_OPERATOR_HPP

