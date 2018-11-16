#ifndef INCLUDED_MEEVAX_LISP_LIST_HPP
#define INCLUDED_MEEVAX_LISP_LIST_HPP

#include <cassert>
#include <iterator>
#include <typeindex>
#include <typeinfo>
#include <utility>

#include <meevax/lisp/cell.hpp>
#include <meevax/tuple/accessor.hpp>
#include <meevax/utility/type_erasure.hpp>

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

  // TODO move to symbol table utility header
  decltype(auto) intern(const std::string& s, std::unordered_map<std::string, cursor>& table)
  {
    if (const auto iter {table.find(s)}; iter != std::end(table))
    {
      return iter->second;
    }
    else return table.emplace(
      s, std::make_shared<utility::binder<std::string, cell>>(s)
    ).first->second;
  }

  // TODO move to symbol table utility header
  // returns unchecked reference
  decltype(auto) lookup(const std::string& s, const std::unordered_map<std::string, cursor>& table)
  {
    const auto iter {table.find(s)};
    assert(iter != std::end(table));
    return iter->second;
  }

  auto cons = [](auto&& head, auto&& tail)
    -> cursor
  {
    return std::make_shared<cell>(head, tail);
  };

  template <typename T, typename U>
  decltype(auto) operator|(T&& head, U&& tail)
  {
    return cons(std::forward<T>(head), std::forward<U>(tail));
  }

  template <typename T>
  decltype(auto) atom(T&& e)
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
    return (args | ... | lookup("nil", symbols));
  };

  decltype(auto) length(const cursor& exp)
  {
    return std::distance(exp, lookup("nil", symbols));
  }

  cursor append(const cursor& x, const cursor& y)
  {
    return !x ? y : car(x) | append(cdr(x), y);
  }

  cursor zip(const cursor& x, const cursor& y)
  {
    if (!x && !y)
    {
      return lookup("nil", symbols);
    }
    else if (!atom(x) && !atom(y))
    {
      return list(car(x), car(y)) | zip(cdr(x), cdr(y));
    }
    else
    {
      return lookup("nil", symbols);
    }
  }

  cursor lookup(const cursor& var, const cursor& env)
  {
    if (!var || !env)
    {
      return lookup("nil", symbols);
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

