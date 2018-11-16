#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <functional>
#include <mutex>
#include <unordered_map>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lambda/recursion.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/closure.hpp>
#include <meevax/lisp/exception.hpp>
#include <meevax/lisp/list.hpp>
#include <meevax/lisp/writer.hpp> // to_string

namespace meevax::lisp
{
  class evaluator
  {
    cursor env_;

    using procedure = std::function<cursor (const cursor&, const cursor&)>;

    // TODO rename to "primitives"?
    static inline std::unordered_map<
      std::shared_ptr<cell>, procedure
    > procedures {};

    std::mutex mutex_;

  public:
    evaluator()
      : env_ {lookup("nil", symbols)}
    {
      intern("true", symbols);

      define("quote", [&](auto&& exp, auto)
      {
        return cadr(exp);
      });

      define("atom", [&](auto&& exp, auto&& env)
      {
        return lookup(
          atom(evaluate(cadr(exp), env)) ? "true" : "nil",
          symbols
        );
      });

      define("eq", [&](auto&& exp, auto&& env)
      {
        return lookup(
          evaluate(cadr(exp), env) == evaluate(caddr(exp), env) ? "true" : "nil",
          symbols
        );
      });

      define("if", [&](auto&& exp, auto&& env)
      {
        return evaluate(
          evaluate(cadr(exp), env) ? caddr(exp) : cadddr(exp),
          env
        );
      });

      define("cond", [&](auto&& exp, auto&& env)
      {
        const auto buffer {
          std::find_if(cdr(exp), lookup("nil", symbols), [&](auto iter)
          {
            return evaluate(car(iter), env);
          })
        };
        return evaluate(cadar(buffer), env);
      });

      define("car", [&](auto&& exp, auto&& env)
      {
        return car(evaluate(cadr(exp), env));
      });

      define("cdr", [&](auto&& exp, auto&& env)
      {
        return cdr(evaluate(cadr(exp), env));
      });

      define("cons", [&](auto&& e, auto&& a)
      {
        return evaluate(cadr(e), a) | evaluate(caddr(e), a);
      });

      define("lambda", [&](auto&& exp, auto&& env)
      {
        using binder = utility::binder<closure, cell>;
        return std::make_shared<binder>(exp, env);
      });

      define("define", [&](auto&& var, auto)
      {
        return lookup(
          cadr(var),
          env_ = list(cadr(var), caddr(var)) | env_
        );
      });

      define("list", [&](auto&& exp, auto&& env)
      {
        return lambda::y([&](auto&& proc, auto&& exp, auto&& env) -> cursor
        {
          return evaluate(car(exp), env) | (cdr(exp) ? proc(proc, cdr(exp), env) : lookup("nil", symbols));
        })(cdr(exp), env);
      });

      define("exit", [&](auto, auto)
        -> const cursor&
      {
        std::exit(boost::exit_success);
      });
    }

    template <typename T>
    decltype(auto) operator()(T&& e)
    {
      return evaluate(std::forward<T>(e), env_);
    }

    template <typename S, typename F>
    void define(S&& s, F&& functor)
    {
      std::lock_guard<std::mutex> lock {mutex_};
      procedures.emplace(intern(s, symbols), functor);
    }

  protected:
    cursor evaluate(const cursor& exp, cursor env) const
    {
      if (atom(exp))
      {
        return lookup(exp, env);
      }
      // primitive procedure?
      else if (auto iter {procedures.find(car(exp))}; iter != std::end(procedures))
      {
        return (iter->second)(exp, env);
      }
      else if (auto callee {evaluate(car(exp), env)}; callee)
      {
        if (callee->type() == typeid(closure))
        {
          // apply compound procedure
          return apply(callee->as<closure>(), cdr(exp), env);
        }
        else
        {
          return evaluate(callee | cdr(exp), env);
        }
      }
      else throw generate_exception(
        "unexpected evaluation dispatch failure for expression " + to_string(exp)
      );
    }

    cursor apply(const closure& closure, const cursor& args, const cursor& env) const
    {
      return evaluate(caddar(closure), append(zip(cadar(closure), evlis(args, env)), cdr(closure)));
    }

    cursor evlis(const cursor& exp, const cursor& env) const
    {
      return !exp ? lookup("nil", symbols) : evaluate(car(exp), env) | evlis(cdr(exp), env);
    }
  }
#ifndef MEEVAX_DISABLE_IMPLICIT_STATIC_EVALUATOR_INSTANTIATION
  static eval {};
#endif
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

