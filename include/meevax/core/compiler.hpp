#ifndef INCLUDED_MEEVAX_CORE_COMPILER_HPP
#define INCLUDED_MEEVAX_CORE_COMPILER_HPP

#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

#include <meevax/core/context.hpp>
#include <meevax/core/instruction.hpp>
#include <meevax/core/number.hpp>
#include <meevax/core/operator.hpp>
#include <meevax/core/pair.hpp>

namespace meevax::core
{
  class compiler
  {
    std::unordered_map<
      std::shared_ptr<pair>,
      std::function<const cursor&, const cursor&, const cursor&>
    > syntaxes;

  public:
    // Intern syntax symbols to given package.
    // スペシャルフォームとVMインストラクションの対応はコンパイラ内部で完結した処理で、
    // スペシャルフォームのシンボルはパッケージを通じてリーダが文字列と対応付ける。
    explicit compiler(context& package)
    {
      // TODO Check number of arguments

      define_syntax(package.intern("quote"), [&](auto&& exp, auto&&, auto&& continuation)
      {
        return cons(LDC, cadr(exp), continuation);
      });

      define_syntax(package.intern("if"), [&](auto&& exp, auto&& env, auto&& continuation)
      {
        const auto&& then_exp {compile( caddr(exp), env, list(JOIN))};
        const auto&& else_exp {compile(cadddr(exp), env, list(JOIN))}; // TODO check cdddr is not nil
        return compile(cadr(exp), env, cons(SELECT, then_exp, else_exp, continuation));
      });

      define_syntax(package.intern("define"), [&](auto&& exp, auto&& env, auto&& continuation)
      {
        return compile(caddr(exp), env, cons(DEFINE, cadr(exp), continuation));
      });

      define_syntax(package.intern("lambda"), [&](auto&& exp, auto&& env, auto&& continuation)
      {
        return cons(
          LDF,
          begin(
            cddr(exp), // If caddr(exp), disabled implicit begin.
            cons(cadr(exp), env), // Connect lambda parameters to current environment.
            list(RETURN)
          ),
          continuation
        );
      });

      // TODO Eliminate the distinction between the instruction and the symbol interned in the package.
      define_syntax(package.intern("car"), [&](auto&& exp, auto&& env, auto&& continuation)
      {
        return compile(cadr(exp), env, cons(CAR, continuation));
      });

      // TODO Eliminate the distinction between the instruction and the symbol interned in the package.
      define_syntax(package.intern("cdr"), [&](auto&& exp, auto&& env, auto&& continuation)
      {
        return compile(cadr(exp), env, cons(CDR, continuation));
      });

      // TODO Eliminate the distinction between the instruction and the symbol interned in the package.
      define_syntax(package.intern("cons"), [&](auto&& exp, auto&& env, auto&& continuation)
      {
        return compile(
          caddr(exp), // Next of the second argument of cons
          env,
          compile(cadr(exp), env, cons(CONS, continuation))
        );
      });
    }

    decltype(auto) operator()(const cursor& exp)
    {
      return compile(exp, nil, list(STOP));
    }

    // auto [iterator_to_defined_syntax, succeeded] = define_syntax(...);
    template <typename... Ts>
    decltype(auto) define_syntax(const cursor& keyword, Ts&&... args)
    {
      return syntaxes.emplace(keyword, std::forward<Ts>(args)...);
    }

  protected:
    cursor compile(const cursor& exp,
                   const cursor& env,
                   const cursor& continuation)
    {
      if (not exp)
      {
        return cons(LDC, nil, continuation); // TODO Add NIL instruction?
      }
      else if (not exp.is<pair>())
      {
        if (exp.is<std::string>()) // is variable
        {
          if (auto location {locate(exp, env)}; location)
          {
            return cons(LDX, location, continuation);
          }
          else
          {
            return cons(LDG, exp, continuation);
          }
        }
        else // is self-evaluation
        {
          return cons(LDC, exp, continuation);
        }
      }
      else // is syntax or arguments
      {
        if (const auto syntax {syntaxes.find(car(exp))}; syntax != std::end(syntaxes))
        {
          return std::get<1>(*syntax)(exp, env, continuation);
        }
        else
        {
          return args(cdr(exp), env, compile(car(exp), env, cons(APPLY, continuation)));
        }
      }
    }

    cursor begin(const cursor& exp,
                 const cursor& env,
                 const cursor& continuation)
    {
      return compile(car(exp), env,
        cdr(exp) ? cons(POP, begin(cdr(exp), env, continuation))
                 :                                continuation
      );
    }

    cursor args(const cursor& exp,
                const cursor& env,
                const cursor& continuation)
    {
      if (exp && exp.is<pair>())
      {
        return args(cdr(exp), env,
          compile(car(exp), env, cons(CONS, continuation))
        );
      }
      else
      {
        return compile(exp, env, continuation);
      }
    }

    cursor locate(const cursor& exp, const cursor& env)
    {
      auto i {0}, j {0};

      for (auto x {env}; x; ++x, ++i)
      {
        for (auto y {car(x)}; y; ++y, ++j)
        {
          if (y.is<pair>() && car(y) == exp)
          {
            return cons(cursor::bind<number>(i), cursor::bind<number>(j));
          }

          if (!y.is<pair>() && y == exp)
          {
            return cons(cursor::bind<number>(i), cursor::bind<number>(-++j));
          }
        }
      }

      return nil;
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_COMPILER_HPP

