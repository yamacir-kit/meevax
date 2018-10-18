#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <functional>
#include <unordered_map>
#include <utility>
#include <vector>

#include <boost/cstdlib.hpp>

#include <meevax/functional/combinator.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/exception.hpp>
#include <meevax/lisp/writer.hpp> // to_string

namespace meevax::lisp
{
  class evaluator
  {
    cursor env_, nil_, true_;

    std::unordered_map<
      std::shared_ptr<cell>,
      std::function<cursor (cursor, cursor)>
    > procedure;

  public:
    evaluator()
      : env_ {symbols.unchecked_reference("nil")},
        nil_ {symbols.unchecked_reference("nil")},
        true_ {symbols.intern("true")}
    {
      define("quote", [](auto e, auto)
      {
        return *++e;
      });

      define("atom", [&](auto e, auto a)
      {
        return atom(eval(*++e, a)) ? true_ : nil_;
      });

      define("eq", [&](auto e, auto a)
      {
        return eval(*++e, a) == eval(*++e, a) ? true_ : nil_;
      });

      define("if", [&](auto e, auto a)
      {
        return eval(*++e, a) ? eval(cadr(e), a) : eval(caddr(e), a);
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
        return assoc(cadr(e), env_ = list(cadr(e), caddr(e)) | env_);
      });

      using namespace meevax::functional;

      define("list", [&](auto e, auto a)
      {
        return z([&](auto proc, auto e, auto a) -> cursor
        {
          return eval(*e, a) | (cdr(e) ? proc(proc, cdr(e), a) : nil_);
        })(++e, a);
      });

      define("exit", [&](auto, auto)
        -> cursor
      {
        std::exit(boost::exit_success);
      });
    }

    template <typename T>
    decltype(auto) operator()(T&& e)
    {
      return eval(std::forward<T>(e), env_);
    }

    template <typename S, typename F>
    void define(S&& s, F&& functor)
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
        return eval(caddar(e), append(zip(cadar(e), evlis(cdr(e), a)), a));
      }
      else if (**e == symbols.intern("macro"))
      {
        // ((macro (params...) (body...)) args...)

        const auto expanded {eval(caddar(e), append(zip(cadar(e), cdr(e)), a))};
        //                        ~~~~~~~~~             ~~~~~~~~  ~~~~~~
        //                        ^ body                ^ params  ^ args

        std::cerr << "-> " << expanded << std::endl;

        return eval(expanded, a);
      }
      else throw generate_exception(
        "unexpected evaluation dispatch failure for expression " + to_string(e)
      );
    }

  private:
    cursor invoke(cursor sexp, cursor alis) noexcept(false)
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
        throw generate_exception("using unbound symbol " + to_string(*sexp) + " as procedure");
      }
    };

    template <typename... Ts>
    cursor list(Ts&&... args)
    {
      return (args | ... | nil_);
    };

    cursor append(cursor x, cursor y)
    {
      return !x ? y : *x | append(cdr(x), y);
    }

    cursor zip(cursor x, cursor y)
    {
      if (!x && !y)
      {
        return nil_;
      }
      else if (!atom(x) && !atom(y))
      {
        return list(*x, *y) | zip(cdr(x), cdr(y));
      }
      else
      {
        return nil_;
      }
    }

    cursor assoc(cursor sexp, cursor alis)
    {
      return !sexp or !alis ? nil_ : sexp == **alis ? cadar(alis) : assoc(sexp, cdr(alis));
    }

    cursor evcon(cursor sexp, cursor alis)
    {
      return eval(**sexp, alis) ? eval(cadar(sexp), alis) : evcon(++sexp, alis);
    }

    cursor evlis(cursor m, cursor a)
    {
      return !m ? nil_ : eval(*m, a) | evlis(cdr(m), a);
    }
  } static eval {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

