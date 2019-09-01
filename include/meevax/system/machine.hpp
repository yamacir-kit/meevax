#ifndef INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP
#define INCLUDED_MEEVAX_SYSTEM_MACHINE_HPP

#include <functional> // std::invoke

#include <meevax/system/boolean.hpp> // false_object
#include <meevax/system/closure.hpp>
#include <meevax/system/continuation.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/instruction.hpp>
#include <meevax/system/iterator.hpp>
#include <meevax/system/numerical.hpp>
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
          d; // dump (continuation)

  public:
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

    /*
     *〈expression〉=〈identifier〉        => LOAD_GLOBAL / LOAD_LOCAL / LOAD_LOCAL_VARIADIC
     *              |〈literal〉           => LOAD_LITERAL
     *              |〈procedure call〉
     *              |〈lambda expression〉
     *              |〈conditional〉
     *              |〈assignment〉
     *              |〈derived expression〉
     */
    object compile(const object& expression,
                   const object& lexical_environment = unit,
                   const object& continuation = list(_stop_), bool tail = false)
    {
      if (not expression)
      {
        return cons(_load_literal_, unit, continuation);
      }
      else if (not expression.is<pair>())
      {
        TRACE("compile") << expression << " ; => ";

        if (expression.is<symbol>()) // is variable
        {
          if (de_bruijn_index index {expression, lexical_environment}; index)
          {
            // XXX デバッグ用のトレースがないなら条件演算子でコンパクトにまとめたほうが良い
            if (index.is_variadic())
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
            std::cerr << "is global variable => " << list(_load_global_, expression) << std::endl;
            return cons(_load_global_, expression, continuation);
          }
        }
        else
        {
          std::cerr << "is self-evaluation => " << list(_load_literal_, expression) << std::endl;
          return cons(_load_literal_, expression, continuation);
        }
      }
      else // is (application . arguments)
      {
        if (const object& buffer {assoc(car(expression), interaction_environment())}; !buffer)
        {
          TRACE("compile") << "(" << car(expression) << " ; => is application of unit => ERROR" << std::endl;
          throw error {"unit is not applicable"}; // TODO syntax-error
        }
        else if (buffer != unbound && buffer.is<special>() && not de_bruijn_index(car(expression), lexical_environment))
        {
          TRACE("compile") << "(" << car(expression) << " ; => is application of " << buffer << std::endl;
          NEST_IN;
          auto result {std::invoke(buffer.as<special>(), cdr(expression), lexical_environment, continuation, tail)};
          NEST_OUT;
          return result;
        }
        else if (buffer != unbound && buffer.is<Environment>() && not de_bruijn_index(car(expression), lexical_environment))
        {
          TRACE("compile") << "(" << car(expression) << " ; => is use of " << buffer << " => " << std::flush;

          auto& macro {assoc(car(expression), interaction_environment()).template as<Environment&>()};
          auto expanded {macro.expand(cdr(expression))};
          TRACE("macroexpand") << expanded << std::endl;

          NEST_IN;
          auto result {compile(expanded, lexical_environment, continuation)};
          NEST_OUT;
          return result;
        }
        else // is (closure . arguments)
        {
          TRACE("compile") << "( ; => is any application " << std::endl;

          NEST_IN;
          auto result {operand(
                   cdr(expression),
                   lexical_environment,
                   compile(
                     car(expression),
                     lexical_environment,
                     cons(tail ? _apply_tail_ : _apply_, continuation)
                   )
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
      switch (c.top().as<instruction>().value)
      {
      case code::LOAD_LOCAL: // S E (LOAD_LOCAL (i . j) . C) D => (value . S) E C D
        DEBUG(2);
        {
          iterator region {e};
          std::advance(region, int {caadr(c).as<real>()});

          iterator position {*region};
          std::advance(position, int {cdadr(c).as<real>()});

          s.push(*position);
        }
        c.pop(2);
        goto dispatch;

      case code::LOAD_LOCAL_VARIADIC:
        DEBUG(2);
        {
          iterator region {e};
          std::advance(region, int {caadr(c).as<real>()});

          iterator position {*region};
          std::advance(position, int {cdadr(c).as<real>()});

          s.push(position);
        }
        c.pop(2);
        goto dispatch;

      case code::LOAD_LITERAL: // S E (LOAD_LITERAL constant . C) D => (constant . S) E C D
        DEBUG(2);
        s.push(cadr(c));
        c.pop(2);
        goto dispatch;

      case code::LOAD_GLOBAL: // S E (LOAD_GLOBAL symbol . C) D => (value . S) E C D
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

      case code::MAKE_ENVIRONMENT: // S E (MAKE_ENVIRONMENT code . C) => (enclosure . S) E C D
        DEBUG(2);
        s.push(make<Environment>(cadr(c), interaction_environment()));
        c.pop(2);
        goto dispatch;

      case code::MAKE_CLOSURE: // S E (MAKE_CLOSURE code . C) => (closure . S) E C D
        DEBUG(2);
        s.push(make<closure>(cadr(c), e));
        c.pop(2);
        goto dispatch;

      case code::MAKE_CONTINUATION: // S E (MAKE_CONTINUATION code . C) D => ((continuation) . S) E C D
        DEBUG(2);
        s.push(list(make<continuation>(s, cons(e, cadr(c), d)))); // XXX 本当は cons(s, e, cadr(c), d) としたいけど、make<continuation> の引数はペア型の引数である必要があるため歪な形になってる。
        c.pop(2);
        goto dispatch;

      case code::SELECT: // (boolean . S) E (SELECT then else . C) D => S E then/else (C . D)
        DEBUG(3);
        d.push(cdddr(c));
        c = car(s) != false_object ? cadr(c) : caddr(c);
        s.pop(1);
        goto dispatch;

      case code::SELECT_TAIL:
        DEBUG(3);
        c = car(s) != false_object ? cadr(c) : caddr(c);
        s.pop(1);
        goto dispatch;

      case code::JOIN: // S E (JOIN . x) (C . D) => S E C D
        DEBUG(1);
        c = car(d);
        d.pop(1);
        goto dispatch;

      case code::DEFINE:
        DEBUG(2);
        define(cadr(c), car(s));
        car(s) = cadr(c); // return value of define
        c.pop(2);
        goto dispatch;

      case code::APPLY:
        DEBUG(1);

        if (auto callee {car(s)}; not callee)
        {
          static const error e {"unit is not appliciable"};
          throw e;
        }
        else if (callee.is<closure>()) // (closure args . S) E (APPLY . C) D
        {
          d.push(cddr(s), e, cdr(c));
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure args . S) E (APPLY . C) D => (result . S) E C D
        {
          s = std::invoke(callee.as<procedure>(), cadr(s)) | cddr(s);
          c.pop(1);
        }
        else if (callee.is<continuation>()) // (continuation args . S) E (APPLY . C) D
        {
          s = cons(caadr(s), car(callee));
          e = cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
        }
        else
        {
          throw error {"\x1b[31m", callee, "\x1b[31m", " is not applicable"};
        }
        goto dispatch;

      case code::APPLY_TAIL:
        DEBUG(1);

        if (auto callee {car(s)}; not callee)
        {
          throw error {"unit is not appliciable"};
        }
        else if (callee.is<closure>()) // (closure args . S) E (APPLY . C) D
        {
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure args . S) E (APPLY . C) D => (result . S) E C D
        {
          s = std::invoke(callee.as<procedure>(), cadr(s)) | cddr(s);
          c.pop(1);
        }
        else if (callee.is<continuation>()) // (continuation args . S) E (APPLY . C) D
        {
          s = cons(caadr(s), car(callee));
          e = cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
        }
        else
        {
          throw error {"\x1b[31m", callee, "\x1b[31m", " is not applicable"};
        }
        goto dispatch;

      case code::RETURN: // (value . S) E (RETURN . C) (S' E' C' . D) => (value . S') E' C' D
        DEBUG(1);
        s = cons(car(s), d.pop());
        e = d.pop();
        c = d.pop();
        goto dispatch;

      case code::PUSH:
        DEBUG(1);
        s = car(s) | cadr(s) | cddr(s);
        c.pop(1);
        goto dispatch;

      case code::POP: // (var . S) E (POP . C) D => S E C D
        DEBUG(1);
        s.pop(1);
        c.pop(1);
        goto dispatch;

      case code::SET_GLOBAL: // (value . S) E (SET_GLOBAL symbol . C) D => (value . S) E C D
        DEBUG(2);
        // TODO
        // (1) There is no need to make copy if right hand side is unique.
        // (2) There is no matter overwrite if left hand side is unique.
        // (3) Should set with weak reference if right hand side is newer.
        std::atomic_store(&unsafe_assoc(cadr(c), interaction_environment()), car(s).copy());
        c.pop(2);
        goto dispatch;

      case code::SET_LOCAL: // (value . S) E (SET_LOCAL (i . j) . C) D => (value . S) E C D
        DEBUG(2);
        {
          iterator region {e};
          std::advance(region, int {caadr(c).as<real>()});

          iterator position {*region};
          std::advance(position, int {cdadr(c).as<real>()});

          std::atomic_store(&car(position), car(s));
        }
        c.pop(2);
        goto dispatch;

      case code::SET_LOCAL_VARIADIC:
        DEBUG(2);
        {
          iterator region {e};
          std::advance(region, int {caadr(c).as<real>()});

          iterator position {*region};
          std::advance(position, int {cdadr(c).as<real>()} - 1);

          std::atomic_store(&cdr(position), car(s));
        }
        c.pop(2);
        goto dispatch;

      default:
      case code::STOP: // (result . S) E (STOP . C) D
        DEBUG(1);
        c.pop(1);
        return s.pop(); // car(s);
      }
    }

    class de_bruijn_index
      : public object // for runtime
    {
      bool variadic;

    public:
      template <typename... Ts>
      de_bruijn_index(Ts&&... xs)
        : object {locate(std::forward<Ts>(xs)...)}
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
              variadic = false;
              return cons(make<real>(i), make<real>(j));
            }
            else if (not position.is<pair>() && position == variable)
            {
              variadic = true;
              return cons(make<real>(i), make<real>(j));
            }

            ++j;
          }

          ++i;
        }

        return unit;
      }

      bool is_variadic() const noexcept
      {
        return variadic;
      }
    };

  protected: // syntax
    /*
     * <quotation> = (quote <datum>)
     */
    object quotation(const object& expression,
                     const object&,
                     const object& continuation, bool)
    {
      TRACE("compile") << car(expression) << "\t; <datum>" << std::endl;
      return cons(_load_literal_, car(expression), continuation);
    }

    /*
     * <sequence> = <command>* <expression>
     *
     * <command> = <expression (the return value will be ignored)>
     */
    object sequence(const object& expression,
                    const object& lexical_environment,
                    const object& continuation,
                    const bool optimization = false)
    {
      if (not cdr(expression)) // is tail sequence
      {
        return compile(
                 car(expression),
                 lexical_environment,
                 continuation,
                 optimization
               );
      }
      else
      {
        return compile(
                 car(expression), // first expression
                 lexical_environment,
                 cons(
                   _pop_, // pop first expression
                   sequence(
                     cdr(expression), // rest expressions
                     lexical_environment,
                     continuation
                   )
                 )
               );
      }
    }

    /*
     * <program> = <command or definition>* <expression>+
     *
     * <command or definition> = <command>
     *                         | <definition>
     *                         | (begin <command or definition>+)
     *
     * <command> = <expression (the return value will be ignored)>
     */
    object program(const object& expression,
                   const object& lexical_environment,
                   const object& continuation, const bool = false) try
    {
      if (not cdr(expression)) // is tail sequence
      {
        return compile(car(expression), lexical_environment, continuation, true);
      }
      else
      {
        return compile(
                 car(expression),
                 lexical_environment,
                 cons(_pop_, sequence(cdr(expression), lexical_environment, continuation))
               );
      }
    }
    catch (const error&) // XXX <definition> backtrack
    {
      // (environment (<unknown>)
      //   <car expression>
      //   <cdr expression>
      //   )
      //
      // <car expression> = (<caar expression> <cadar expression> <caddar expression>)

      return compile(
               cddar(expression) ? caddar(expression) : undefined,
               lexical_environment,
               cons(
                 _define_, cadar(expression),
                 cdr(expression) ? program(
                                     cdr(expression),
                                     lexical_environment,
                                     continuation
                                   )
                                 : continuation
               )
             );
    }

    /*
     * <definition> = (define <identifier> <expression>)
     */
    object definition(const object& expression,
                      const object& lexical_environment,
                      const object& continuation,
                      const bool = false)
    {
      if (not lexical_environment)
      {
        TRACE("compile") << car(expression) << "\t; => <variable>" << std::endl;

        return compile(
                 cdr(expression) ? cadr(expression) : undefined,
                 lexical_environment,
                 cons(_define_, car(expression), continuation)
               );
      }
      else
      {
        // TODO COMPILE_ERROR
        throw error {"syntax-error at internal-define"};
      }
    }

    /*
     * <body> = <definition>* <sequence>
     */
    object body(const object& expression,
                const object& lexical_environment,
                const object& continuation, bool = false) try
    {
      if (not cdr(expression)) // is tail sequence
      {
        return compile(car(expression), lexical_environment, continuation, true);
      }
      else
      {
        return compile(
                 car(expression),
                 lexical_environment,
                 cons(_pop_, sequence(cdr(expression), lexical_environment, continuation))
               );
      }
    }
    catch (const error&) // <definition> backtrack
    {
      stack bindings {};

      /*
       * <sequence> = <command>* <expression>
       */
      for (iterator sequences {expression}; sequences; ++sequences)
      {
        /*
         * <definition> = (define <identifier> <expression>)
         */
        if (const object& definition {car(sequences)}; car(definition).as<symbol>() == "define")
        {
          /*
           * <binding> = (<identifier> <expression>)
           */
          bindings.push(cdr(definition));
        }
        else
        {
          /*
           * At least one binding assumed. Because, this catch block execution
           * will be triggered by encountering the <definition> on compiling
           * rule <sequence>.
           */
          assert(not bindings.empty());

          const object& identifier {
            static_cast<Environment&>(*this).intern("letrec*")
          };

          if (const object& internal_define {assoc(identifier, interaction_environment())};
              internal_define and internal_define.is<Environment>())
          {
            /*
             * (letrec* (<binding>+) <sequence>+)
             */
            const auto& transformer {internal_define.as<Environment>().expand(
              cons(bindings, sequences)
            )};

            return compile(transformer, lexical_environment, continuation);
          }
          else
          {
            throw error {"internal-define requires derived expression \"letrec*\" (This inconvenience will be resolved in the future)"};
          }
        }
      }

      // auto binding_specs {list()};
      // auto non_definitions {unit};
      //
      // for (iterator iter {expression}; iter; ++iter)
      // {
      //   if (const object operation {car(*iter)}; operation.as<symbol>() == "define")
      //   {
      //     // std::cerr << "[INTERNAL DEFINE] " << cdr(*iter) << std::endl;
      //     binding_specs = cons(cdr(*iter), binding_specs);
      //   }
      //   else
      //   {
      //     non_definitions = iter;
      //     break;
      //   }
      // }
      //
      // // std::cerr << binding_specs << std::endl;
      // // std::cerr << cons(binding_specs, non_definitions) << std::endl;
      //
      // object letrec_star {assoc(
      //   static_cast<Environment&>(*this).intern("letrec*"),
      //   interaction_environment()
      // )};
      //
      // if (not letrec_star or not letrec_star.is<Environment>())
      // {
      //   throw error {"internal-define requires derived expression \"letrec*\" (This inconvenience will be resolved in the future)"};
      // }
      //
      // auto expanded {letrec_star.as<Environment>().expand(
      //   cons(binding_specs, non_definitions)
      // )};
      //
      // // std::cerr << expanded << std::endl;
      //
      // return compile(expanded, lexical_environment, continuation);
    }

    /*
     * <operand> = <expression>
     */
    object operand(const object& expression,
                   const object& lexical_environment,
                   const object& continuation, bool = false)
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

    // [[deprecated]]
    // object let(const object& expression,
    //            const object& lexical_environment,
    //            const object& continuation)
    // {
    //   const auto binding_specs {car(expression)};
    //
    //   const auto identifiers {
    //     map([](auto&& e) { return car(e); }, binding_specs)
    //   };
    //
    //   const auto initializations {
    //     map([](auto&& e) { return cadr(e); }, binding_specs)
    //   };
    //
    //   return operand(
    //            initializations,
    //            lexical_environment,
    //            cons(
    //              _make_closure_,
    //              body(
    //                cdr(expression), // <body>
    //                cons(identifiers, lexical_environment),
    //                list(_return_)
    //              ),
    //              _apply_, continuation
    //            )
    //          );
    // }

    object set(const object& expression,
               const object& lexical_environment,
               const object& continuation, bool = false)
    {
      TRACE("compile") << car(expression) << " ; => is ";

      if (!expression)
      {
        throw error {"syntax error at #(special set!)"};
      }
      else if (de_bruijn_index index {car(expression), lexical_environment}; index)
      {
        // XXX デバッグ用のトレースがないなら条件演算子でコンパクトにまとめたほうが良い
        if (index.is_variadic())
        {
          std::cerr << " local variadic variable => " << list(_set_local_variadic_, index) << std::endl;

          return compile(
                   cadr(expression),
                   lexical_environment,
                   cons(_set_local_variadic_, index, continuation)
                 );
        }
        else
        {
          std::cerr << " local variable => " << list(_set_local_, index) << std::endl;

          return compile(
                   cadr(expression),
                   lexical_environment,
                   cons(_set_local_, index, continuation)
                 );
        }
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

