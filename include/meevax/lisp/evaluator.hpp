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
#include <meevax/lisp/operator.hpp>
#include <meevax/lisp/table.hpp>
#include <meevax/lisp/writer.hpp> // to_string

namespace meevax::lisp
{
  class evaluator
  {
    cursor env_;

    using procedure = std::function<cursor (const cursor&, const cursor&)>;
    static inline std::unordered_map<std::shared_ptr<cell>, procedure> procedures {};

    std::mutex mutex_;

  public:
    evaluator()
      : env_ {nil}
    {
      define("quote", [&](auto&& exp, auto)
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
        return assoc(
          cadr(var),
          env_ = list(cadr(var), caddr(var)) | env_
        );
      });

      define("list", [&](auto&& exp, auto&& env)
      {
        return lambda::y([&](auto&& proc, auto&& exp, auto&& env) -> cursor
        {
          return evaluate(car(exp), env) | (cdr(exp) ? proc(proc, cdr(exp), env) : nil);
        })(cdr(exp), env);
      });

      define("exit", [&](auto, auto)
        -> const cursor&
      {
        std::exit(boost::exit_success);
      });
    }

    template <typename Expression>
    constexpr decltype(auto) operator()(Expression&& exp)
    {
      return evaluate(std::forward<Expression>(exp), env_);
    }

    template <typename String, typename Function>
    void define(String&& s, Function&& functor)
    {
      std::lock_guard<std::mutex> lock {mutex_};
      procedures.emplace(intern(s, symbols), functor);
    }

  protected:
    template <typename Expression, typename Environment>
    cursor evaluate(Expression&& exp, Environment&& env)
    {
      if (atom(exp))
      {
        return assoc(exp, env);
      }
      else if (const auto& iter {procedures.find(car(exp))}; iter != std::end(procedures))
      {
        return (iter->second)(exp, env);
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
      else throw generate_exception(
        "unexpected evaluation dispatch failure for expression " + to_string(exp)
      );
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
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

