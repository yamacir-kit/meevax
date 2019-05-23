#ifndef INCLUDED_MEEVAX_SYSTEM_OPERATOR_HPP
#define INCLUDED_MEEVAX_SYSTEM_OPERATOR_HPP

#include <sstream>

#include <meevax/system/pair.hpp>
#include <meevax/system/exception.hpp>

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

namespace meevax::system
{
  // For C++17 fold-expression
  template <typename T, typename U>
  objective operator|(T&& lhs, U&& rhs)
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

  template <typename T, typename U>
  objective append(T&& x, U&& y)
  {
    return !x ? y : car(x) | append(cdr(x), std::forward<U>(y));
  }

  objective zip(const objective& x, const objective& y)
  {
    if (!x && !y)
    {
      return unit;
    }
    else if (x.is<pair>() && y.is<pair>())
    {
      return list(car(x), car(y)) | zip(cdr(x), cdr(y));
    }
    else
    {
      return unit;
    }
  }

  template <typename... Ts>
  decltype(auto) display(Ts&&... args)
  {
    return (std::cout << ... << args) << std::endl;
  }

  template <typename... Ts>
  std::string pseudo_display(Ts&&... args) // TODO RENAME TO LEXICAL_CAST
  {
    std::stringstream buffer {};
    (buffer << ... << args);
    return buffer.str();
  }

  // 統合案
  // template <typename OutputStream, typename... Ts>
  // decltype(auto) display(OutputStream&& os, Ts&&... args)
  // {
  //   return (os << ... << args);
  // }

  const objective& assoc(const objective& var, const objective& env)
  {
    if (!var)
    {
      return unit;
    }
    if (!env)
    {
      return unbound;
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

  [[deprecated]] std::ostream& display_assoc(std::ostream& os, const objective& env)
  {
    if (!env)
    {
      return os;
    }
    else
    {
      display_assoc(os, cdr(env));
      return os << "\t" << caar(env) << "\r\x1b[40C " << cadar(env) << std::endl;
    }
  }

  objective& unsafe_assoc(const objective& var, objective& env) noexcept(false)
  {
    assert(var);

    if (!env)
    {
      throw error {pseudo_display(var, "\x01b[31m", " is unbound")};
    }
    else if (caar(env) == var)
    {
      return cadar(env);
    }
    else
    {
      return unsafe_assoc(var, cdr(env));
    }
  }

  objective take(const objective& exp, std::size_t size)
  {
    if (0 < size)
    {
      return car(exp) | take(cdr(exp), --size);
    }
    else
    {
      return unit;
    }
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_OPERATOR_HPP

