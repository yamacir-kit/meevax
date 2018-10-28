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
#include <meevax/lisp/list.hpp>
#include <meevax/lisp/writer.hpp> // to_string

namespace meevax::lisp
{
  class evaluator
  {
    cursor env_;

    using procedure = std::function<cursor (const cursor&, const cursor&)>;
    std::unordered_map<std::shared_ptr<cell>, procedure> procedures;

    // TODO
    // プライベートメンバ関数をクロージャクラスのメンバに変更すること
    // クロージャクラスを独立したヘッダに移動すること
    struct closure
    {
      cursor exp, env;
    };

  public:
    evaluator()
      : env_ {symbols("nil")}
    {
      using namespace meevax::functional;

      define("quote", [](auto e, auto)
      {
        return *++e;
      });

      define("atom", [&](auto e, auto a)
      {
        return atom(evaluate(*cdr(e), a)) ? symbols.intern("true") : symbols("nil");
      });

      define("eq", [&](auto e, auto a)
      {
        return evaluate(*++e, a) == evaluate(*++e, a) ? symbols.intern("true") : symbols("nil");
      });

      define("if", [&](auto e, auto a)
      {
        return evaluate(*++e, a) ? evaluate(cadr(e), a) : evaluate(caddr(e), a);
      });

      define("cond", [&](auto exp, auto env)
      {
        return z([&](auto&& proc, auto&& exp, auto&& env) -> cursor
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
        return z([&](auto proc, auto e, auto a) -> cursor
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
      procedures.emplace(symbols.intern(s), functor);
    }

  protected:
    cursor evaluate(const cursor& exp, cursor env)
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
          const auto closure {callee.access().as<evaluator::closure>()};
          return evaluate(caddr(closure.exp), append(zip(cadr(closure.exp), evlis(cdr(exp), env)), closure.env));
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

  private:
    cursor evlis(const cursor& exp, const cursor& env)
    {
      return !exp ? symbols("nil")
                   : evaluate(car(exp), env) | evlis(cdr(exp), env);
    }
  } static eval {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_EVALUATOR_HPP

