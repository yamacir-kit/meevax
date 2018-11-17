#ifndef INCLUDED_MEEVAX_LISP_LIST_HPP
#define INCLUDED_MEEVAX_LISP_LIST_HPP

#include <iterator>
#include <typeindex>
#include <typeinfo>
#include <utility>

#include <meevax/lisp/cell.hpp>
#include <meevax/tuple/accessor.hpp>

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

  auto cons = [](auto&& head, auto&& tail)
  {
    // XXX ここで余分なコピーが発生してる説
    return cursor {std::make_shared<cell>(head, tail)};
  };

  template <typename T, typename U>
  decltype(auto) operator|(T&& head, U&& tail)
  {
    return cons(std::forward<T>(head), std::forward<U>(tail));
  }

  decltype(auto) atom(const cursor& e)
  {
    static const std::unordered_map<std::type_index, bool> dispatch
    {
      {typeid(cell), false},
      {typeid(std::string), true}
    };

    return !e || dispatch.at(e->type());
  }

  auto list = [](auto&&... args) constexpr
  {
    return (args | ... | nil);
  };

  decltype(auto) length(const cursor& exp)
  {
    return std::distance(exp, nil);
  }

  cursor append(const cursor& x, const cursor& y)
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

  const cursor& lookup(const cursor& var, const cursor& env)
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
      return lookup(var, cdr(env));
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_LIST_HPP

