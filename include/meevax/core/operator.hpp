#ifndef INCLUDED_MEEVAX_CORE_OPERATOR_HPP
#define INCLUDED_MEEVAX_CORE_OPERATOR_HPP

#include <iterator>
#include <typeinfo>
#include <utility>

#include <meevax/core/pair.hpp>

// TODO 純LISPコアに必要なものは移設すること

#define caar(...) car(car(__VA_ARGS__))
#define cadr(...) car(cdr(__VA_ARGS__))
#define cdar(...) cdr(car(__VA_ARGS__))
#define cddr(...) cdr(cdr(__VA_ARGS__))

#define caaar(...) car(caar(__VA_ARGS__))
#define caadr(...) car(cadr(__VA_ARGS__))
#define cadar(...) car(cdar(__VA_ARGS__))
#define caddr(...) car(cddr(__VA_ARGS__))
#define cdaar(...) cdr(caar(__VA_ARGS__))
#define cdadr(...) cdr(cadr(__VA_ARGS__))
#define cddar(...) cdr(cdar(__VA_ARGS__))
#define cdddr(...) cdr(cddr(__VA_ARGS__))

#define caaaar(...) car(caaar(__VA_ARGS__))
#define caaadr(...) car(caadr(__VA_ARGS__))
#define caadar(...) car(cadar(__VA_ARGS__))
#define caaddr(...) car(caddr(__VA_ARGS__))
#define cadaar(...) car(cdaar(__VA_ARGS__))
#define cadadr(...) car(cdadr(__VA_ARGS__))
#define caddar(...) car(cddar(__VA_ARGS__))
#define cadddr(...) car(cdddr(__VA_ARGS__))
#define cdaaar(...) cdr(caaar(__VA_ARGS__))
#define cdaadr(...) cdr(caadr(__VA_ARGS__))
#define cdadar(...) cdr(cadar(__VA_ARGS__))
#define cdaddr(...) cdr(caddr(__VA_ARGS__))
#define cddaar(...) cdr(cdaar(__VA_ARGS__))
#define cddadr(...) cdr(cdadr(__VA_ARGS__))
#define cdddar(...) cdr(cddar(__VA_ARGS__))
#define cddddr(...) cdr(cdddr(__VA_ARGS__))

namespace meevax::core
{
  // For C++17 fold-expression
  template <typename T, typename U>
  constexpr cursor operator|(T&& lhs, U&& rhs)
  {
    return std::make_shared<pair>(std::forward<T>(lhs), std::forward<U>(rhs));
  }

  template <typename... Ts>
  constexpr decltype(auto) cons(Ts&&... args)
  {
    return (args | ...);
  }

  template <typename... Ts>
  constexpr decltype(auto) list(Ts&&... args)
  {
    return (args | ... | unit);
  }

  decltype(auto) atom(const cursor& exp)
  {
    return !exp || !exp.is<pair>();
  }

  // decltype(auto) length(const cursor& exp)
  // {
  //   return std::distance(exp, unit);
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
      return unit;
    }
    else if (!atom(x) && !atom(y))
    {
      return list(car(x), car(y)) | zip(cdr(x), cdr(y));
    }
    else
    {
      return unit;
    }
  }

  cursor assoc(const cursor& var, const cursor& env)
  {
    if (!var)
    {
      return var;
    }
    else if (!env)
    {
      return undefined;
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
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_OPERATOR_HPP

