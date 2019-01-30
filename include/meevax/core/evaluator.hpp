#ifndef INCLUDED_MEEVAX_CORE_EVALUATOR_HPP
#define INCLUDED_MEEVAX_CORE_EVALUATOR_HPP

#include <functional>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/core/closure.hpp>
#include <meevax/core/context.hpp>
#include <meevax/core/operator.hpp>
#include <meevax/core/pair.hpp>
#include <meevax/core/reader.hpp>
#include <meevax/lambda/curry.hpp>
#include <meevax/lambda/recursion.hpp>

namespace meevax::core
{
  // For builtin procedures.
  // Dispatch using std::unordered_map and std::function is flexible but, may be too slowly.
  struct procedure
    : public std::function<cursor (cursor&, cursor&)>
  {
    template <typename... Ts>
    explicit constexpr procedure(Ts&&... args)
      : std::function<cursor (cursor&, cursor&)> {std::forward<Ts>(args)...}
    {}

    // XXX
    // Change signature to receive evaluator as first argument?
    // It is not necessary if we request to use A lambda expression capturing
    // instance of the evaluator defined in the main function for defining procedures.
  };

  // Evaluator is a functor provides eval-apply cycle, also holds builtin procedure table.
  class evaluator
    : public std::unordered_map<std::shared_ptr<pair>, procedure>
  {
    cursor env_;
    context context_;

  public:
    // TODO need more constructor.
    evaluator(); // The definition is at the end of this file.

    // Assign primitive procedure to dispatch table with it's name.
    template <typename String, typename Function>
    void define(String&& s, Function&& function)
    {
      emplace(context_.intern(s), std::forward<Function>(function));
    }

    decltype(auto) operator()(cursor& exp)
    {
      return evaluate(exp, env_);
    }

    decltype(auto) operator()(const std::string& s)
    {
      return evaluate(read(context_, s), env_);
    }

    decltype(auto) reader()
    {
      return lambda::curry(read)(context_);
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
        if (callee.template is<closure>())
        {
          return apply(callee, cdr(exp), env);
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
      auto&& buffer {evaluate(cadr(exp), env)};
      return buffer ? car(buffer) : nil;
    });

    define("cdr", [&](auto&& exp, auto&& env)
    {
      auto&& buffer {evaluate(cadr(exp), env)};
      return buffer ? cdr(buffer) : nil;
    });

    define("cons", [&](auto&& exp, auto&& env)
    {
      return evaluate(cadr(exp), env) | evaluate(caddr(exp), env);
    });

    define("lambda", [&](auto&&... args)
    {
      return cursor::bind<closure>(std::forward<decltype(args)>(args)...);
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
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_EVALUATOR_HPP

