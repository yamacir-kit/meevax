#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <functional>
#include <unordered_map>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lambda/recursion.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/closure.hpp>
#include <meevax/lisp/context.hpp>
#include <meevax/lisp/exception.hpp>
#include <meevax/lisp/operator.hpp>
#include <meevax/lisp/writer.hpp> // to_string

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
    evaluator()
      : env_ {nil}
    {
      #include <meevax/lisp/procedure.hpp>
    }

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

