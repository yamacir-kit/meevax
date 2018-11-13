#ifndef INCLUDED_MEEVAX_LISP_EVALUATOR_HPP
#define INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

#include <functional>
#include <mutex>
#include <unordered_map>
#include <utility>

#include <boost/cstdlib.hpp>

#include <meevax/lambda/recursion.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/exception.hpp>
#include <meevax/lisp/list.hpp>
#include <meevax/lisp/writer.hpp> // to_string

namespace meevax::lisp
{
  class evaluator
  {
    cursor env_;

    using procedure = std::function<cursor (const cursor&, const cursor&)>;
    static inline std::unordered_map<std::shared_ptr<cell>, procedure> procedures {};

    std::mutex mutex_;

    class closure
    {
      cursor exp_, env_;

    public:
      explicit closure(const cursor& exp, const cursor& env)
        : exp_ {exp},
          env_ {env}
      {}

      decltype(auto) operator()(const cursor& args, const cursor& env) const
      {
        return evaluate(caddr(exp_), append(zip(cadr(exp_), evlis(args, env)), env_));
      }

    protected:
      cursor evlis(const cursor& exp, const cursor& env) const
      {
        return !exp ? symbols("nil") : evaluate(car(exp), env) | evlis(cdr(exp), env);
      }
    };

  public:
    evaluator()
      : env_ {symbols("nil")}
    {
      using namespace meevax::lambda;

      symbols.intern("true");

      define("quote", [](auto exp, auto)
      {
        return cadr(exp);
      });

      define("atom", [&](auto exp, auto env)
      {
        return atom(evaluate(cadr(exp), env)) ? symbols("true") : symbols("nil");
      });

      define("eq", [&](auto e, auto a)
      {
        return evaluate(cadr(e), a) == evaluate(caddr(e), a) ? symbols("true") : symbols("nil");
      });

      define("if", [&](auto e, auto a)
      {
        return evaluate(*++e, a) ? evaluate(cadr(e), a) : evaluate(caddr(e), a);
      });

      define("cond", [&](auto exp, auto env)
      {
        return y([&](auto&& proc, auto&& exp, auto&& env) -> cursor
        {
          return evaluate(**exp, env) ? evaluate(cadar(exp), env) : proc(proc, ++exp, env);
        })(++exp, env);
      });

      define("car", [&](auto e, auto a)
      {
        return *evaluate(*++e, a);
      });

      define("cdr", [&](auto e, auto a)
      {
        return ++evaluate(*++e, a);
      });

      define("cons", [&](auto e, auto a)
      {
        return evaluate(cadr(e), a) | evaluate(caddr(e), a);
      });

      define("lambda", [&](auto exp, auto env)
      {
        using binder = utility::binder<closure, cell>;
        return std::make_shared<binder>(exp, env);
      });

      define("define", [&](auto value, auto)
      {
        return lookup(cadr(value), env_ = list(cadr(value), caddr(value)) | env_);
      });

      define("list", [&](auto e, auto a)
      {
        return y([&](auto proc, auto e, auto a) -> cursor
        {
          return evaluate(*e, a) | (cdr(e) ? proc(proc, cdr(e), a) : symbols("nil"));
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
      return evaluate(std::forward<T>(e), env_);
    }

    template <typename S, typename F>
    void define(S&& s, F&& functor)
    {
      std::lock_guard<std::mutex> lock {mutex_};
      procedures.emplace(symbols.intern(s), functor);
    }

  protected:
    static cursor evaluate(const cursor& exp, cursor env)
    {
      if (atom(exp))
      {
        return lookup(exp, env);
      }

      if (auto iter {procedures.find(car(exp))}; iter != std::end(procedures))
      {
        return (iter->second)(exp, env);
      }

      if (auto callee {evaluate(car(exp), env)}; callee)
      {
        if (callee.access().type() == typeid(closure))
        {
          return callee.access().as<closure>()(cdr(exp), env);
        }
        else
        {
          return evaluate(callee | cdr(exp), env);
        }
      }

      throw generate_exception(
        "unexpected evaluation dispatch failure for expression " + to_string(exp)
      );
    }
  }
#ifndef MEEVAX_DISABLE_IMPLICIT_STATIC_EVALUATOR_INSTANTIATION
  static eval {};
#endif
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

