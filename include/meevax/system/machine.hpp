#ifndef INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP

#include <functional> // std::invoke

#include <meevax/system/boolean.hpp> // _false_
#include <meevax/system/closure.hpp>
#include <meevax/system/continuation.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/instruction.hpp>
#include <meevax/system/iterator.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/procedure.hpp>
#include <meevax/system/special.hpp>
#include <meevax/system/srfi-1.hpp> // assoc
#include <meevax/system/stack.hpp>
#include <meevax/system/symbol.hpp> // object::is<symbol>()
#include <meevax/utility/debug.hpp>

#define DEBUG(N) // std::cerr << "; machine\t; " << "\x1B[?7l" << take(c, N) << "\x1B[?7h" << std::endl

namespace meevax::system
{
  template <typename Environment>
  class machine // Simple SECD machine.
  {
  protected:
    stack s, // main stack
          e, // lexical environment
          c, // control
          d; // dump

  public:
    machine()
      : s {unit}
      , e {unit}
      , c {unit}
      , d {unit}
    {}

    decltype(auto) interaction_environment()
    {
      return static_cast<Environment&>(*this).interaction_environment();
    }

    // Direct virtual machine instruction invocation.
    template <typename... Ts>
    decltype(auto) define(const object& key, Ts&&... args)
    {
      interaction_environment().push(list(key, std::forward<Ts>(args)...));
      std::cerr << "; define\t; " << caar(interaction_environment()) << "\r\x1b[40C\x1b[K " << cadar(interaction_environment()) << std::endl;
      return interaction_environment(); // temporary
    }

    template <bool Elimination = false>
    object compile(const object& exp,
                   const object& scope = unit,
                   const object& continuation = list(_stop_))
    {
      if (not exp)
      {
        return cons(_load_literal_, unit, continuation);
      }
      else if (not exp.is<pair>())
      {
        TRACE("compile") << exp << " ; => ";

        if (exp.is<symbol>()) // is variable
        {
          if (de_bruijn_index index {exp, scope}; index)
          {
            if (index.is_tail())
            {
              std::cerr << "is local variadic variable => " << list(_load_local_variadic_, index) << std::endl;
              return cons(_load_local_variadic_, index, continuation);
            }
            else
            {
              std::cerr << "is local variable => " << list(_load_local_, index) << std::endl;
              return cons(_load_local_, index, continuation);
            }
          }
          else
          {
            std::cerr << "is global variable => " << list(_load_global_, exp) << std::endl;
            return cons(_load_global_, exp, continuation);
          }
        }
        else // is self-evaluation
        {
          std::cerr << "is self-evaluation => " << list(_load_literal_, exp) << std::endl;
          return cons(_load_literal_, exp, continuation);
        }
      }
      else // is (application . arguments)
      {
        if (const object& buffer {assoc(car(exp), interaction_environment())}; !buffer)
        {
          TRACE("compile") << "(" << car(exp) << " ; => is application of unit => ERROR" << std::endl;
          throw error {"unit is not applicable"}; // TODO syntax-error
        }
        else if ((/* std::cerr << "." << std::flush, */ buffer != unbound) &&
                 (/* std::cerr << "." << std::flush, */ buffer.is<special>()) &&
                 (/* std::cerr << "." << std::flush, */ not de_bruijn_index(car(exp), scope)))
        {
          TRACE("compile") << "(" << car(exp) << " ; => is application of ";
          std::cerr << buffer << std::endl;
          NEST_IN;
          auto result {std::invoke(buffer.as<special>(), cdr(exp), scope, continuation)};
          NEST_OUT;
          return result;
        }
        else if ((/* std::cerr << "." << std::flush, */ buffer != unbound) &&
                 (/* std::cerr << "." << std::flush, */ buffer.is<Environment>()) &&
                 (/* std::cerr << "." << std::flush, */ not de_bruijn_index(car(exp), scope)))
        {
          TRACE("compile") << "(" << car(exp) << " ; => is use of " << buffer << " => " << std::flush;

          auto& macro {unsafe_assoc(car(exp), interaction_environment()).template as<Environment&>()};
          // auto expanded {macro.expand(cdr(exp), interaction_environment())};
          auto expanded {macro.expand(cdr(exp))};
          TRACE("macroexpand") << expanded << std::endl;

          NEST_IN;
          auto result {compile(expanded, scope, continuation)};
          NEST_OUT;
          return result;
        }
        else // is (closure . arguments)
        {
          TRACE("compile") << "( ; => is any application " << std::endl;

          NEST_IN;
          auto result {operand(
                   cdr(exp),
                   scope,
                   compile(car(exp), scope, cons(_apply_, continuation))
                 )};
          NEST_OUT;
          return result;
        }
      }
    }

    decltype(auto) execute(const object& expression)
    {
      c = expression;
      std::cerr << "; machine\t; " << c << std::endl;
      return execute();
    }

    object execute()
    {
    dispatch:
      switch (c.top().as<instruction>().code)
      {
      case secd::LOAD_LOCAL: // S E (LOAD_LOCAL (i . j) . C) D => (value . S) E C D
        {
          DEBUG(2);

          // Distance to target stack frame from current stack frame.
          int i {caadr(c).as<number>()};

          // Index of target value in the target stack frame.
          // If value is lower than 0, the target value is variadic parameter.
          int j {cdadr(c).as<number>()};

          // TODO Add LDV (load-variadic) instruction to remove this conditional.
          if (iterator region {car(std::next(e, i))}; j < 0)
          {
            s.push(std::next(region, -(j + 1)));
          }
          else
          {
            s.push(car(std::next(region, j)));
          }
        }
        c.pop(2);
        goto dispatch;

      case secd::LOAD_LOCAL_VARIADIC:
        DEBUG(2);
        {
          iterator region {e};
          std::advance(region, int {caadr(c).as<number>()});

          iterator position {*region};
          int distance {cdadr(c).as<number>()};
          std::advance(position, -(distance + 1));

          s.push(position);
        }
        c.pop(2);
        goto dispatch;

      case secd::LOAD_LITERAL: // S E (LOAD_LITERAL constant . C) D => (constant . S) E C D
        DEBUG(2);
        s.push(cadr(c));
        c.pop(2);
        goto dispatch;

      case secd::LOAD_GLOBAL: // S E (LOAD_GLOBAL symbol . C) D => (value . S) E C D
        DEBUG(2);
        if (auto value {assoc(cadr(c), interaction_environment())}; value != unbound)
        {
          s.push(value);
        }
        else
        {
          throw error {cadr(c), " is unbound"};
        }
        c.pop(2);
        goto dispatch;

      case secd::MAKE_MODULE: // S E (MAKE_MODULE code . C) => (enclosure . S) E C D
        DEBUG(2);
        s.push(make<Environment>(cadr(c), interaction_environment())); // レキシカル環境が必要ないのかはよく分からん
        c.pop(2);
        goto dispatch;

      case secd::MAKE_CLOSURE: // S E (MAKE_CLOSURE code . C) => (closure . S) E C D
        DEBUG(2);
        s.push(make<closure>(cadr(c), e));
        c.pop(2);
        goto dispatch;

      case secd::MAKE_CONTINUATION: // S E (MAKE_CONTINUATION code . C) D => ((continuation) . S) E C D
        DEBUG(2);

        // XXX 本当は cons(s, e, cadr(c), d) としたいけど、make<continuation> の引数はペア型の引数である必要があるため歪な形になってる。
        s.push(list(make<continuation>(s, cons(e, cadr(c), d))));
        c.pop(2);
        goto dispatch;

      case secd::SELECT: // (boolean . S) E (SELECT then else . C) D => S E then/else (C . D)
        DEBUG(3);
        d.push(cdddr(c));
        c = car(s) != _false_ ? cadr(c) : caddr(c);
        s.pop(1);
        goto dispatch;

      case secd::JOIN: // S E (JOIN . x) (C . D) => S E C D
        DEBUG(1);
        c = car(d);
        d.pop(1);
        goto dispatch;

      case secd::DEFINE:
        DEBUG(2);
        define(cadr(c), car(s));
        car(s) = cadr(c); // return value of define (change to #<undefined>?)
        c.pop(2);
        goto dispatch;

      case secd::STOP: // (result . S) E (STOP . C) D
        DEBUG(1);
        c.pop(1);
        return s.pop(); // car(s);

      case secd::APPLY:
        DEBUG(1);

        if (auto applicable {car(s)}; not applicable)
        {
          throw error {"unit is not appliciable"};
        }
        else if (applicable.is<closure>()) // (closure args . S) E (APPLY . C) D
        {
          d.push(cddr(s), e, cdr(c));
          c = car(applicable);
          e = cons(cadr(s), cdr(applicable));
          s = unit;
        }
        else if (applicable.is<procedure>()) // (procedure args . S) E (APPLY . C) D => (result . S) E C D
        {
          s = std::invoke(applicable.as<procedure>(), cadr(s)) | cddr(s);
          c.pop(1);
        }
        else if (applicable.is<continuation>()) // (continuation args . S) E (APPLY . C) D
        {
          s = cons(caadr(s), car(applicable));
          e = cadr(applicable);
          c = caddr(applicable);
          d = cdddr(applicable);
        }
        else
        {
          throw error {applicable, "\x1b[31m", " is not applicable"};
        }
        goto dispatch;

      case secd::RETURN: // (value . S) E (RETURN . C) (S' E' C' . D) => (value . S') E' C' D
        DEBUG(1);
        s = cons(car(s), d.pop());
        e = d.pop();
        c = d.pop();
        goto dispatch;

      case secd::PUSH:
        DEBUG(1);
        s = cons(cons(car(s), cadr(s)), cddr(s)); // s = car(s) | cadr(s) | cddr(s);
        c.pop(1);
        goto dispatch;

      case secd::POP: // (var . S) E (POP . C) D => S E C D
        DEBUG(1);
        s.pop(1);
        c.pop(1);
        goto dispatch;

      case secd::SET_GLOBAL: // (value . S) E (SET_GLOBAL symbol . C) D => (value . S) E C D
        DEBUG(2);
        // TODO
        // (1) There is no need to make copy if right hand side is unique.
        // (2) There is no matter overwrite if left hand side is unique.
        // (3) Should set with weak reference if right hand side is newer.
        std::atomic_store(&unsafe_assoc(cadr(c), interaction_environment()), car(s).copy());
        c.pop(2);
        goto dispatch;

      case secd::SET_LOCAL: // (var . S) E (SET_LOCAL (i . j) . C) D => (var . S) E C D
        {
          DEBUG(2);

          // Distance to target stack frame from current stack frame.
          int i {caadr(c).as<number>()};

          // Index of target value in the target stack frame.
          // If value is lower than 0, the target value is variadic parameter.
          int j {cdadr(c).as<number>()};

          // TODO Add SETV (set-variadic) instruction to remove this conditional.
          auto tmp {e};

          while (0 < i--)
          {
            tmp = cdr(tmp);
          }

          if (auto region {car(tmp)}; j < 0)
          {
            // std::next(scope, -++j) <= car(s);
            auto var {region};
            while (++j < -1) // ここ自信ない（一つ多いか少ないかも）
            {
              var = cdr(var);
            }
            std::atomic_store(&var, car(s));
          }
          else
          {
            // car(std::next(scope, j)) <= car(s);
            auto var {region};
            while (0 < j--)
            {
              var = cdr(var);
            }
            std::atomic_store(&car(var), car(s));
          }
        }
        c.pop(2);
        goto dispatch;

      default:
        // XXX この式、実行されない（switchの方チェックの時点で例外で出て行く）
        throw error {car(c), "\x1b[31m is not virtual machine instruction"};
      }

      // XXX この式、実行されない（そもそもたどり着かない）
      throw error {"unterminated execution"};
    }

    // TODO 内部的にランタイム数値オブジェクトじゃない形でインデックスを持つべき
    struct de_bruijn_index
      : public object // for runtime
    {
      de_bruijn_index(const object& variable,
                      const object& lexical_environment)
        : object {locate(variable, lexical_environment)}
      {}

      object locate(const object& variable,
                    const object& lexical_environment)
      {
        auto i {0};

        for (const auto& region : lexical_environment)
        {
          auto j {0};

          for (iterator position {region}; position; ++position)
          {
            if (position.is<pair>() && *position == variable)
            {
              return cons(make<number>(i), make<number>(j));
            }
            else if (not position.is<pair>() && position == variable)
            {
              // メモ：取り出すときは j < 0 の間イテレータを進める感じで
              return cons(make<number>(i), make<number>(-1 - j));
            }

            ++j;
          }

          ++i;
        }

        return unit;
      }

      bool is_tail() const noexcept
      {
        return cdr(*this).template as<number>() < 0;
      }
    };

  protected:
    /* 7.1.3
     * sequence = command* expression
     * command = expression
     */
    object sequence(const object& expression,
                    const object& region,
                    const object& continuation)
    {
      return compile(
               car(expression),
               region,
               cdr(expression) ? cons(
                                   _pop_,
                                   sequence(cdr(expression), region, continuation)
                                 )
                               : continuation
             );
    }

    /*  7.1.3
     *
     * <body> = <definition>* <sequence>
     *
     */
    object body(const object& expression,
                const object& lexical_environment,
                const object& continuation) try
    {
      return compile(
               car(expression),
               lexical_environment,
               cdr(expression) ? cons(
                                   _pop_,
                                   sequence(cdr(expression), lexical_environment, continuation)
                                 )
                               : continuation
             );
    }
    catch (const error&) // internal define backtrack (DIRTY HACK)
    {
      auto binding_specs {list()};
      auto non_definitions {unit};

      for (iterator iter {expression}; iter; ++iter)
      {
        if (const object operation {car(*iter)}; operation.as<symbol>() == "define")
        {
          // std::cerr << "[INTERNAL DEFINE] " << cdr(*iter) << std::endl;
          binding_specs = cons(cdr(*iter), binding_specs);
        }
        else
        {
          non_definitions = iter;
          break;
        }
      }

      // std::cerr << binding_specs << std::endl;
      // std::cerr << cons(binding_specs, non_definitions) << std::endl;

      object letrec_star {assoc(
        static_cast<Environment&>(*this).intern("letrec*"),
        interaction_environment()
      )};

      auto expanded {letrec_star.as<Environment>().expand(
        cons(binding_specs, non_definitions)
      )};

      // std::cerr << expanded << std::endl;

      return compile(expanded, lexical_environment, continuation);
    }

    /* 7.1.3
     *
     * <operand> = <expression>
     *
     */
    object operand(const object& expression,
                   const object& lexical_environment,
                   const object& continuation)
    {
      if (expression && expression.is<pair>())
      {
        return operand(
                 cdr(expression),
                 lexical_environment,
                 compile(car(expression), lexical_environment, cons(_push_, continuation))
               );
      }
      else
      {
        return compile(expression, lexical_environment, continuation);
      }
    }

    [[deprecated]]
    object let(const object& expression,
               const object& lexical_environment,
               const object& continuation)
    {
      const auto binding_specs {car(expression)};

      const auto identifiers {
        map([](auto&& e) { return car(e); }, binding_specs)
      };

      const auto initializations {
        map([](auto&& e) { return cadr(e); }, binding_specs)
      };

      return operand(
               initializations,
               lexical_environment,
               cons(
                 _make_closure_,
                 body(
                   cdr(expression), // <body>
                   cons(identifiers, lexical_environment),
                   list(_return_)
                 ),
                 _apply_, continuation
               )
             );
    }

    object set(const object& expression,
               const object& lexical_environment,
               const object& continuation)
    {
      TRACE("compile") << car(expression) << " ; => is ";

      if (!expression)
      {
        throw error {"syntax error at #<special set!>"};
      }
      else if (de_bruijn_index index {car(expression), lexical_environment}; index)
      {
        std::cerr << " local variable => " << list(_set_local_, index) << std::endl;

        return compile(
                 cadr(expression),
                 lexical_environment,
                 cons(_set_local_, index, continuation)
               );
      }
      else
      {
        std::cerr << " global variable => " << list(_set_global_, car(expression)) << std::endl;

        return compile(
                 cadr(expression),
                 lexical_environment,
                 cons(_set_global_, car(expression), continuation)
               );
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP

