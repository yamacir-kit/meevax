#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/error.hpp>
#include <meevax/lisp/table.hpp>
#include <meevax/lisp/writer.hpp>

#define cdr(e) std::next(e)

#define caar(e) **e
#define cadar(e) *std::next(*e)
#define caddar(e) *std::next(*e, 2)

#define cadr(e) *std::next(e)
#define caddr(e) *std::next(e, 2)
#define cadddr(e) *std::next(e, 3)

namespace meevax::lisp
{
  std::unordered_map<
    std::shared_ptr<cell>,
    std::function<cursor (cursor, cursor)>
  > procedure;

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
        return *eval(*++e, a);
      });

      define("cdr", [&](auto e, auto a)
      {
        return ++eval(*++e, a);
      });

      define("cons", [&](auto e, auto a)
      {
        return eval(cadr(e), a) | eval(caddr(e), a);
      });

      define("define", [&](auto e, auto)
      {
        env = list(cadr(e), caddr(e)) | env;
        return assoc(cadr(e), env);
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
    void define(const std::string& s, F&& functor)
    {
      procedure.emplace(symbols.intern(s), functor);
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
        return invoke(e, a);
      }
      else if (**e == symbols.intern("recursive"))
      {
        return eval(caddar(e) | cdr(e), list(cadar(e), *e) | a);
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
    cursor invoke(cursor sexp, cursor alis)
    {
      if (const auto callee {assoc(*sexp, alis)}; callee) // user defined procedure
      {
        return eval(callee | cdr(sexp), alis);
      }
      else try
      {
        return procedure.at(*sexp)(sexp, alis);
      }
      catch (const std::out_of_range& error)
      {
        std::cerr << error("using unbound symbol " << *sexp << " as procedure") << std::endl;
        return symbols.intern("nil");
      }
    };

    static constexpr auto list = [](auto&&... args)
    {
      return (args | ... | symbols.intern("nil"));
    };

    cursor append(cursor x, cursor y)
    {
      return !x ? y : *x | append(cdr(x), y);
    }

    cursor zip(cursor x, cursor y)
    {
      if (!x && !y)
      {
        return symbols.intern("nil");
      }
      else if (!atom(x) && !atom(y))
      {
        return list(*x, *y) | zip(cdr(x), cdr(y));
      }
      else
      {
        return symbols.intern("nil");
      }
    }

    cursor assoc(cursor sexp, cursor alis)
    {
      return !sexp or !alis ? symbols.intern("nil") : sexp == **alis ? cadar(alis) : assoc(sexp, cdr(alis));
    }

    cursor evcon(cursor sexp, cursor alis)
    {
      return eval(**sexp, alis) ? eval(cadar(sexp), alis) : evcon(++sexp, alis);
    }

    cursor evlis(cursor m, cursor a)
    {
      return !m ? symbols.intern("nil") : eval(*m, a) | evlis(cdr(m), a);
    }
  } static eval {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

