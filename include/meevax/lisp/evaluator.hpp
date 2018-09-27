#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include <meevax/lisp/alias.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/error.hpp>
#include <meevax/lisp/table.hpp>

#define caar(...) car<0, 0>(__VA_ARGS__)
#define cadar(...) car<0, 1>(__VA_ARGS__)
#define caddar(...) car<0, 2>(__VA_ARGS__)

#define cadr(...) car<1>(__VA_ARGS__)
#define caddr(...) car<2>(__VA_ARGS__)
#define cadddr(...) car<3>(__VA_ARGS__)

namespace meevax::lisp
{
  class evaluator
  {
    static inline auto env {nil};

  public:
    evaluator()
    {
      define("quote", [](auto&& e, auto&&)
      {
        return cadr(e);
      });

      define("atom", [&](auto&& e, auto&& a)
      {
        return atom(eval(cadr(e), a)) ? symbols.intern("true") : nil;
      });

      define("eq", [&](auto&& e, auto&& a)
      {
        return eval(cadr(e), a) == eval(caddr(e), a) ? symbols.intern("true") : nil;
      });

      define("cond", [&](auto&& e, auto&& a)
      {
        return evcon(cdr(e), a);
      });

      define("car", [&](auto&& e, auto&& a)
      {
        return car(eval(cadr(e), a));
      });

      define("cdr", [&](auto&& e, auto&& a)
      {
        return cdr(eval(cadr(e), a));
      });

      define("cons", [&](auto&& e, auto&& a)
      {
        return eval(cadr(e), a) | eval(caddr(e), a);
      });

      define("define", [&](auto&& e, auto&& a)
      {
        env = list(cadr(e), caddr(e)) | env;
        return assoc(cadr(e), a);
      });
    }

    decltype(auto) operator()(cursor& e)
    {
      return eval(e, env);
    }

    template <typename F>
    void define(const std::string& s, F&& proc)
    {
      env = list(symbols.intern(s), make_as<special>(proc)) | env;
    }

  protected:
    auto eval(cursor& e, cursor& a)
      -> cursor
    {
      if (atom(e))
      {
        return assoc(e, a);
      }
      else if (atom(car(e)))
      {
        if (auto proc {assoc(car(e), a)}; proc->type() == typeid(special))
        {
          return proc->as<special>()(e, a);
        }
        else if (atom(proc))
        {
          std::cerr << error("using atom \"" << proc << "\" as procedure") << std::endl;
          return nil;
        }
        else
        {
          return eval(proc | cdr(e), a);
        }
      }
      else if (caar(e) == symbols.intern("label"))
      {
        return eval(caddar(e) | cdr(e), list(cadar(e), car(e)) | a);
      }
      else if (caar(e) == symbols.intern("lambda"))
      {
        return eval(caddar(e), append(zip(cadar(e), evlis(cdr(e), a)), a));
      }
      else
      {
        std::cerr << error("unknown function \"" << car(e) << "\"") << std::endl;
        return nil;
      }
    }

  private:
    template <typename... Ts>
    cursor list(Ts&&... xs)
    {
      return (xs | ... | nil);
    }

    template <typename T, typename U>
    cursor append(T&& x, U&& y)
    {
      return !x ? y : car(x) | append(cdr(x), y);
    }

    template <typename T, typename U>
    cursor zip(T&& x, U&& y)
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

    cursor assoc(cursor& x, cursor& y)
    {
      if (!x)
      {
        return nil;
      }
      else if (!y)
      {
        return x;
      }
      else
      {
        return caar(y) == x ? cadar(y) : assoc(x, cdr(y));
      }
    }

    cursor evcon(cursor& c, cursor& a)
    {
      return eval(caar(c), a) ? eval(cadar(c), a) : evcon(cdr(c), a);
    }

    cursor evlis(cursor& m, cursor& a)
    {
      return !m ? nil : eval(car(m), a) | evlis(cdr(m), a);
    }
  } static eval {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

