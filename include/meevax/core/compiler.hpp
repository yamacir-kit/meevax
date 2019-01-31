#ifndef INCLUDED_MEEVAX_CORE_COMPILER_HPP
#define INCLUDED_MEEVAX_CORE_COMPILER_HPP

#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

#include <meevax/core/context.hpp>
#include <meevax/core/instruction.hpp>
#include <meevax/core/operator.hpp>
#include <meevax/core/pair.hpp>

namespace meevax::core
{
  class compiler
  {
    std::unordered_map<
      std::shared_ptr<pair>,
      std::function
    > syntaxes;

  public:
    explicit compiler(const std::shared_ptr<context>& package)
    {
    }

    decltype(auto) operator()(const cursor& exp)
    {
      return compile(exp, nil, list(STOP));
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
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_COMPILER_HPP

