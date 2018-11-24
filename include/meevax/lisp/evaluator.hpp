#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <functional>
#include <unordered_map>
#include <utility>
#include <stdexcept>

#include <boost/cstdlib.hpp>

#include <meevax/lambda/recursion.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/closure.hpp>
#include <meevax/lisp/context.hpp>
#include <meevax/lisp/operator.hpp>
#include <meevax/lisp/reader.hpp>

namespace meevax::lisp
{
  // For builtin procedures.
  // dispatch using std::unordered_map and std::function is flexible but, may
  // be too slowly.
  struct procedure
    : public std::function<cursor (cursor&, cursor&)>
  {
    template <typename... Ts>
    explicit constexpr procedure(Ts&&... args)
      : std::function<cursor (cursor&, cursor&)> {std::forward<Ts>(args)...}
    {}
  };

  // Evaluator is a functor provides eval-apply cycle,
  // also holds builtin procedure table.
  class evaluator
    : public std::unordered_map<std::shared_ptr<cell>, procedure>
  {
    cursor env_;

  public:
    evaluator(); // The definition is at the end of this file.

    template <typename Expression>
    constexpr decltype(auto) operator()(Expression&& exp)
    {
      return evaluate(std::forward<Expression>(exp), env_);
    }

    // Assign primitive procedure to dispatch table with it's name.
    template <typename String, typename Function>
    void define(String&& s, Function&& functor)
    {
      emplace(default_context.intern(s), functor);
    }

  protected:
    template <typename Expression, typename Environment>
    cursor evaluate(Expression&& exp, Environment&& env)
    {
      if (atom(exp))
      {
        return assoc(exp, env);
      }
      // XXX This dispatching is too slowly but, useful for incremental prototyping.
      else if (const auto& iter {find(car(exp))}; iter != std::end(*this))
      {
        return std::get<1>(*iter)(exp, env);
      }
      else if (const auto& callee {evaluate(car(exp), env)}; callee)
      {
        if (callee->type() == typeid(closure))
        {
          return apply(callee->template as<closure>(), cdr(exp), env);
        }
        else
        {
          return evaluate(callee | cdr(exp), env);
        }
      }
      else throw std::runtime_error {"unexpected evaluation dispatch failure"};
    }

    template <typename Closure, typename Expression, typename Environment>
    cursor apply(Closure&& closure, Expression&& args, Environment&& env)
    {
      return evaluate(caddar(closure), append(zip(cadar(closure), evlis(args, env)), cdr(closure)));
    }

    template <typename Expression, typename Environment>
    cursor evlis(Expression&& exp, Environment&& env)
    {
      return !exp ? nil : evaluate(car(exp), env) | evlis(cdr(exp), env);
    }
  };

  evaluator::evaluator()
    : env_ {nil}
  {
    define("quote", [](auto&& exp, auto)
    {
      return cadr(exp);
    });

    define("atom", [&](auto&& exp, auto&& env)
    {
      return atom(evaluate(cadr(exp), env)) ? t : nil;
    });

    define("eq", [&](auto&& exp, auto&& env)
    {
      return evaluate(cadr(exp), env) == evaluate(caddr(exp), env) ? t : nil;
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
        std::find_if(cdr(exp), nil, [&](auto iter)
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

    define("cons", [&](auto&& exp, auto&& env)
    {
      return evaluate(cadr(exp), env) | evaluate(caddr(exp), env);
    });

    define("lambda", [&](auto&& exp, auto&& env)
    {
      using binder = utility::binder<closure, cell>;
      return std::make_shared<binder>(exp, env);
    });

    define("define", [&](auto&& var, auto)
    {
      return assoc(cadr(var), env_ = list(cadr(var), caddr(var)) | env_);
    });

    define("list", [&](auto&& exp, auto&& env)
    {
      return lambda::y([&](auto&& proc, auto&& exp, auto&& env) -> cursor
      {
        return evaluate(car(exp), env) | (cdr(exp) ? proc(proc, cdr(exp), env) : nil);
      })(cdr(exp), env);
    });

    define("exit", [](auto, auto)
      -> cursor
    {
      std::exit(boost::exit_success);
    });
  }
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

