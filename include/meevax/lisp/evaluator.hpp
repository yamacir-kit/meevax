#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <functional>
#include <iostream>
#include <string>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/error.hpp>
#include <meevax/lisp/table.hpp>

#define car(e) *e
#define cdr(e) std::next(e)

#define caar(e) **e
#define cadar(e) *std::next(*e)
#define caddar(e) *std::next(*e, 2)

#define cadr(e) *std::next(e)
#define caddr(e) *std::next(e, 2)
#define cadddr(e) *std::next(e, 3)

namespace meevax::lisp
{
  using special = const std::function<cursor (cursor, cursor)>;

  class evaluator
  {
    static inline auto env {symbols.intern("nil")};

  public:
    evaluator()
    {
      define("quote", [](auto e, auto)
      {
        return *++e;
      });

      define("atom", [&](auto e, auto a)
      {
        return atom(eval(*++e, a)) ? symbols.intern("true") : symbols.intern("nil");
      });

      define("eq", [&](auto e, auto a)
      {
        return eval(*++e, a) == eval(*++e, a) ? symbols.intern("true") : symbols.intern("nil");
      });

      define("cond", [&](auto e, auto a)
      {
        return evcon(++e, a);
      });

      define("car", [&](auto e, auto a)
      {
        return car(eval(*++e, a));
      });

      define("cdr", [&](auto e, auto a)
      {
        return cdr(eval(*++e, a));
      });

      define("cons", [&](auto e, auto a)
      {
        return eval(cadr(e), a) | eval(caddr(e), a);
      });

      define("define", [&](auto e, auto a)
      {
        env = list(cadr(e), caddr(e)) | env;
        return assoc(cadr(e), a);
      });

      define("exit", [&](auto, auto)
      {
        std::exit(boost::exit_success);
        return symbols.intern("nil");
      });
    }

    template <typename T>
    decltype(auto) operator()(T&& e)
    {
      return eval(std::forward<T>(e), env);
    }

    template <typename F>
    void define(const std::string& s, F&& proc)
    {
      env = list(symbols.intern(s), make_as<special>(proc)) | env;
    }

  protected:
    cursor eval(cursor e, cursor a)
    {
      if (atom(e))
      {
        return assoc(e, a);
      }
      else if (atom(*e))
      {
        return apply(e, a);
      }
      else if (**e == symbols.intern("label"))
      {
        return eval(caddar(e) | cdr(e), list(cadar(e), car(e)) | a);
      }
      else if (**e == symbols.intern("lambda"))
      {
        return eval(
                 caddar(e),
                 append(zip(cadar(e), evlis(cdr(e), a)), a)
               );
      }
      else if (**e == symbols.intern("macro"))
      {
        return eval(
                 eval(
                   caddar(e),
                   append(zip(cadar(e), cdr(a)), a)
                 ),
                 a
               );
      }
      else
      {
        std::cerr << error("eval dispatch failed for " << e) << std::endl;
        return symbols.intern("nil");
      }
    }

  private:
    cursor apply(cursor e, cursor a)
    {
      // XXX UGLY CODE!
      if (auto f {assoc(*e, a)}; f->type() == typeid(special))
      {
        return f->template as<special>()(e, a);
      }
      else if (atom(f))
      {
        std::cerr << error("using atom \"" << f << "\" as procedure") << std::endl;
        return symbols.intern("nil");
      }
      else
      {
        return eval(f | ++e, a);
      }
    };

    static constexpr auto list = [](auto&&... args)
    {
      return (args | ... | symbols.intern("nil"));
    };

    cursor append(cursor x, cursor y)
    {
      return !x ? y : car(x) | append(cdr(x), y);
    }

    cursor zip(cursor x, cursor y)
    {
      if (!x && !y)
      {
        return symbols.intern("nil");
      }
      else if (!atom(x) && !atom(y))
      {
        return list(car(x), car(y)) | zip(cdr(x), cdr(y));
      }
      else
      {
        return symbols.intern("nil");
      }
    }

    cursor assoc(cursor x, cursor y)
    {
      return !x ? symbols.intern("nil") : !y ? x : caar(y) == x ? cadar(y) : assoc(x, cdr(y));
    }

    cursor evcon(cursor c, cursor a)
    {
      return eval(caar(c), a) ? eval(cadar(c), a) : evcon(cdr(c), a);
    }

    cursor evlis(cursor m, cursor a)
    {
      return !m ? symbols.intern("nil") : eval(car(m), a) | evlis(cdr(m), a);
    }
  } static eval {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

