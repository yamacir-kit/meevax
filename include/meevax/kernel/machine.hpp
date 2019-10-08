#ifndef INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
#define INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/native.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/symbol.hpp> // object::is<symbol>()
#include <meevax/kernel/syntax.hpp>

inline namespace dirty_hacks
{
  #define TRACE(N)                                                             \
  if (static_cast<Environment&>(*this).trace == true_object)                   \
  {                                                                            \
    std::cerr << "; machine\t; " << "\x1B[?7l" << take(c, N) << "\x1B[?7h" << std::endl; \
  }

  static std::size_t depth {0};

  #define DEBUG_COMPILE(...)                                                   \
  if (   static_cast<Environment&>(*this).verbose          == true_object      \
      or static_cast<Environment&>(*this).verbose_compiler == true_object)     \
  {                                                                            \
    std::cerr << "; compile\t; " << std::string(depth * 4, ' ') << std::flush << __VA_ARGS__; \
  }

  #define DEBUG_COMPILE_DECISION(...)                                          \
  if (   static_cast<Environment&>(*this).verbose          == true_object      \
      or static_cast<Environment&>(*this).verbose_compiler == true_object)     \
  {                                                                            \
    std::cerr << __VA_ARGS__;                                                  \
  }

  #define DEBUG_MACROEXPAND(...)                                               \
  if (   static_cast<Environment&>(*this).verbose          == true_object      \
      or static_cast<Environment&>(*this).verbose_compiler == true_object)     \
  {                                                                            \
    std::cerr << "; macroexpand\t; " << std::string(depth * 4, ' ') << std::flush << __VA_ARGS__; \
  }

  // TODO REMOVE THIS!!!
  #define DEBUG_COMPILE_SYNTAX(...)                                            \
  if (verbose == true_object or verbose_compiler == true_object)               \
  {                                                                            \
    std::cerr << "; compile\t; " << std::string(depth * 4, ' ') << std::flush << __VA_ARGS__; \
  }

  #define NEST_IN  ++depth
  #define NEST_OUT --depth; DEBUG_COMPILE(")" << std::endl)

  // TODO REMOVE THIS!!!
  #define NEST_OUT_SYNTAX --depth; DEBUG_COMPILE_SYNTAX(")" << std::endl)
}

namespace meevax::kernel
{
  template <typename Environment>
  class machine // Simple SECD machine.
  {
  protected:
    stack s, // main stack
          e, // lexical environment
          c, // control stack
          d; // dump stack (current-continuation)

  private: // CRTP Interfaces
    decltype(auto) interaction_environment()
    {
      return static_cast<Environment&>(*this).interaction_environment();
    }

    template <typename... Ts>
    decltype(auto) intern(Ts&&... operands)
    {
      return static_cast<Environment&>(*this).intern(
               std::forward<decltype(operands)>(operands)...
             );
    }

    // TODO Remove
    template <typename... Ts>
    decltype(auto) export_(Ts&&... operands)
    {
      return static_cast<Environment&>(*this).export_(std::forward<decltype(operands)>(operands)...);
    }

  public:
    // Direct virtual machine instruction invocation.
    template <typename... Ts>
    decltype(auto) define(const object& key, Ts&&... operands)
    {
      auto iter {export_(key, std::forward<decltype(operands)>(operands)...)};
      // interaction_environment().push(list(key, std::forward<decltype(operands)>(operands)...));
      interaction_environment().push(list(iter->first, iter->second));

      if (   static_cast<Environment&>(*this).verbose        == true_object
          or static_cast<Environment&>(*this).verbose_define == true_object)
      {
        std::cerr << "; define\t; " << caar(interaction_environment()) << "\r\x1b[40C\x1b[K " << cadar(interaction_environment()) << std::endl;
      }

      return interaction_environment(); // temporary
    }

    // TODO Change last boolean argument to template parameter (use if constexpr)
    /*
     * <expression> = <identifier>
     *              | <literal>
     *              | (<expression> <expression>*)
     *              | <lambda expression>
     *              | <conditional>
     *              | <assignment>
     *              | <derived expression>
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
        DEBUG_COMPILE(expression << " ; => ");

        if (expression.is<symbol>()) // is variable
        {
          if (de_bruijn_index index {expression, lexical_environment}; index)
          {
            // XXX デバッグ用のトレースがないなら条件演算子でコンパクトにまとめたほうが良い
            if (index.is_variadic())
            {
              DEBUG_COMPILE_DECISION("is local variadic variable => " << list(_load_local_variadic_, index) << std::endl);
              return cons(_load_local_variadic_, index, continuation);
            }
            else
            {
              DEBUG_COMPILE_DECISION("is local variable => " << list(_load_local_, index) << std::endl);
              return cons(_load_local_, index, continuation);
            }
          }
          else
          {
            DEBUG_COMPILE_DECISION("is global variable => " << list(_load_global_, expression) << std::endl);
            return cons(_load_global_, expression, continuation);
          }
        }
        else
        {
          DEBUG_COMPILE_DECISION("is self-evaluation => " << list(_load_literal_, expression) << std::endl);
          return cons(_load_literal_, expression, continuation);
        }
      }
      else // is (application . arguments)
      {
        // TODO Rename to applicant?
        object buffer {assoc(
          car(expression), interaction_environment()
        )};

        if (buffer == unbound) // maybe, car(expression) is <identifier>
        {
          buffer = car(expression);
        }

        if (not buffer)
        {
          DEBUG_COMPILE("(" << car(expression) << " ; => is application of unit => ERROR" << std::endl);
          throw syntax_error {"unit is not applicable"};
        }
        else if (buffer != unbound && buffer.is<syntax>() && not de_bruijn_index(car(expression), lexical_environment))
        {
          DEBUG_COMPILE("(" << car(expression) << " ; => is application of " << buffer << std::endl);
          NEST_IN;
          auto result {std::invoke(buffer.as<syntax>(), cdr(expression), lexical_environment, continuation, tail)};
          NEST_OUT;
          return result;
        }
        else if (buffer != unbound && buffer.is<Environment>() && not de_bruijn_index(car(expression), lexical_environment))
        {
          DEBUG_COMPILE("(" << car(expression) << " ; => is use of " << buffer << std::endl);

          const auto expanded {assoc(
            car(expression),
            interaction_environment()
          // ).template as<Environment&>().expand(cdr(expression))};
          ).template as<Environment&>().expand(expression)};

          DEBUG_MACROEXPAND(expanded << std::endl);

          NEST_IN;
          auto result {compile(expanded, lexical_environment, continuation)};
          NEST_OUT;

          return result;
        }
        else // is (closure . arguments)
        {
          DEBUG_COMPILE("( ; => is any application " << std::endl);

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

      if (   static_cast<Environment&>(*this).verbose         == true_object
          or static_cast<Environment&>(*this).verbose_machine == true_object)
      {
        std::cerr << "; machine\t; " << c << std::endl;
      }

      return execute();
    }

    object execute()
    {
      // TODO
      // 上の引数付き版ではない、execute の直接呼び出しはおそらくマクロ展開にしか使われていないはずで、
      // --debug-macroexpand で下のプリントを有効にするべきだと思うが、
      // ここまでそれを意識して実装していないため保留する
      //
      // std::cerr << "; machine\t; " << c << std::endl;

    dispatch:
      switch (c.top().as<instruction>().value)
      {
      case code::LOAD_LOCAL: // S E (LOAD_LOCAL (i . j) . C) D => (value . S) E C D
        TRACE(2);
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
        TRACE(2);
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
        TRACE(2);
        s.push(cadr(c));
        c.pop(2);
        goto dispatch;

      case code::LOAD_GLOBAL: // S E (LOAD_GLOBAL symbol . C) D => (value . S) E C D
        TRACE(2);
        if (auto value {assoc(cadr(c), interaction_environment())}; value != unbound)
        {
          s.push(value);
        }
        else
        {
          // throw evaluation_error {cadr(c), " is unbound"};

          if (   static_cast<Environment&>(*this).verbose == true_object
              or static_cast<Environment&>(*this).verbose_machine == true_object)
          {
            std::cerr << "; machine\t; instruction " << car(c) << " received undefined variable " << cadr(c) << ".\n"
                      << ";\t\t; start implicit renaming..." << std::endl;
          }

          /*********************************************************************
          * When an undefined symbol is evaluated, it returns a symbol that is
          * guaranteed not to collide with any symbol from the past to the
          * future. This behavior is defined for the macrotransformer.
          *********************************************************************/
          s.push(static_cast<Environment&>(*this).rename(cadr(c)));
        }
        c.pop(2);
        goto dispatch;

      case code::MAKE_ENVIRONMENT: // S E (MAKE_ENVIRONMENT code . C) => (enclosure . S) E C D
        TRACE(2);
        // s.push(make<Environment>(cadr(c), interaction_environment()));
        s.push(make<Environment>(make<closure>(cadr(c), e), interaction_environment()));
        c.pop(2);
        goto dispatch;

      case code::MAKE_CLOSURE: // S E (MAKE_CLOSURE code . C) => (closure . S) E C D
        TRACE(2);
        s.push(make<closure>(cadr(c), e));
        c.pop(2);
        goto dispatch;

      case code::MAKE_CONTINUATION: // S E (MAKE_CONTINUATION code . C) D => ((continuation) . S) E C D
        TRACE(2);
        s.push(list(make<continuation>(s, cons(e, cadr(c), d)))); // XXX 本当は cons(s, e, cadr(c), d) としたいけど、make<continuation> の引数はペア型の引数である必要があるため歪な形になってる。
        c.pop(2);
        goto dispatch;

      case code::SELECT: // (boolean . S) E (SELECT then else . C) D => S E then/else (C . D)
        TRACE(3);
        d.push(cdddr(c));
        c = car(s) != false_object ? cadr(c) : caddr(c);
        s.pop(1);
        goto dispatch;

      case code::SELECT_TAIL:
        TRACE(3);
        c = car(s) != false_object ? cadr(c) : caddr(c);
        s.pop(1);
        goto dispatch;

      case code::JOIN: // S E (JOIN . x) (C . D) => S E C D
        TRACE(1);
        c = car(d);
        d.pop(1);
        goto dispatch;

      case code::DEFINE:
        TRACE(2);
        define(cadr(c), car(s));
        car(s) = cadr(c); // return value of define
        c.pop(2);
        goto dispatch;

      case code::APPLY:
        TRACE(1);

        if (auto callee {car(s)}; not callee)
        {
          static const error e {"unit is not appliciable"};
          throw e;
        }
        else if (callee.is<closure>()) // (closure operands . S) E (APPLY . C) D
        {
          d.push(cddr(s), e, cdr(c));
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<native>()) // (native operands . S) E (APPLY . C) D => (result . S) E C D
        {
          s = std::invoke(callee.as<native>(), cadr(s)) | cddr(s);
          c.pop(1);
        }
        // else if (callee.is<Environment>())
        // {
        //   s = callee.as<Environment>().expand(car(s) | cadr(s)) | cddr(s);
        //   c.pop(1);
        // }
        else if (callee.is<continuation>()) // (continuation operands . S) E (APPLY . C) D
        {
          s = cons(caadr(s), car(callee));
          e = cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
        }
        else
        {
          throw evaluation_error {callee, " is not applicable"};
        }
        goto dispatch;

      case code::APPLY_TAIL:
        TRACE(1);

        if (auto callee {car(s)}; not callee)
        {
          throw evaluation_error {"unit is not appliciable"};
        }
        else if (callee.is<closure>()) // (closure operands . S) E (APPLY . C) D
        {
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<native>()) // (native operands . S) E (APPLY . C) D => (result . S) E C D
        {
          s = std::invoke(callee.as<native>(), cadr(s)) | cddr(s);
          c.pop(1);
        }
        // else if (callee.is<Environment>())
        // {
        //   s = callee.as<Environment>().expand(car(s) | cadr(s)) | cddr(s);
        //   c.pop(1);
        // }
        else if (callee.is<continuation>()) // (continuation operands . S) E (APPLY . C) D
        {
          s = cons(caadr(s), car(callee));
          e = cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
        }
        else
        {
          throw evaluation_error {callee, " is not applicable"};
        }
        goto dispatch;

      case code::RETURN: // (value . S) E (RETURN . C) (S' E' C' . D) => (value . S') E' C' D
        TRACE(1);
        s = cons(car(s), d.pop());
        e = d.pop();
        c = d.pop();
        goto dispatch;

      case code::PUSH:
        TRACE(1);
        s = car(s) | cadr(s) | cddr(s);
        c.pop(1);
        goto dispatch;

      case code::POP: // (var . S) E (POP . C) D => S E C D
        TRACE(1);
        s.pop(1);
        c.pop(1);
        goto dispatch;

      case code::SET_GLOBAL: // (value . S) E (SET_GLOBAL symbol . C) D => (value . S) E C D
        TRACE(2);
        // TODO
        // (1) There is no need to make copy if right hand side is unique.
        // (2) There is no matter overwrite if left hand side is unique.
        // (3) Should set with weak reference if right hand side is newer.
        if (const auto& key_value {assq(cadr(c), interaction_environment())}; key_value != false_object)
        {
          // std::cerr << key_value << std::endl;
          std::atomic_store(&cadr(key_value), car(s).copy());
        }
        else
        {
          throw make<error>(cadr(c), " is unbound");
        }
        c.pop(2);
        goto dispatch;

      case code::SET_LOCAL: // (value . S) E (SET_LOCAL (i . j) . C) D => (value . S) E C D
        TRACE(2);
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
        TRACE(2);
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
        TRACE(1);
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
      de_bruijn_index(Ts&&... operands)
        : object {locate(std::forward<decltype(operands)>(operands)...)}
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
      DEBUG_COMPILE(car(expression) << "\t; <datum>" << std::endl);
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
     * <program> = <command or definition>+
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
      //                  = (#(syntax define) <identifier> <expression>)

      auto definition {compile(
        cddar(expression) ? caddar(expression) : undefined,
        lexical_environment,
        list(_define_, cadar(expression))
      )};

      NEST_OUT;

      return append(
               definition,
               cdr(expression)
                 ? program(cdr(expression), lexical_environment, continuation)
                 : continuation
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
        DEBUG_COMPILE(car(expression) << "\t; => <variable>" << std::endl);

        return compile(
                 cdr(expression) ? cadr(expression) : undefined,
                 lexical_environment,
                 cons(_define_, car(expression), continuation)
               );
      }
      else
      {
        throw syntax_error_about_internal_define {
          "definition cannot appear in this context"
        };
      }
    }

    /*
     * <body> := <definition>* <sequence>
     */
    object body(const object& expression,
                const object& lexical_environment,
                const object& continuation, bool = false) // try
    {
      /************************************************************************
      * The expression may have following form.
      *
      * (lambda (...)
      *   <definition or command> ;= <car expression>
      *   <definition or sequence> ;= <cdr expression>
      *   )
      ************************************************************************/
      if (not cdr(expression)) // is tail sequence
      {
        /**********************************************************************
        * The expression may have following form.
        * If definition appears in <car expression>, it is an syntax error.
        *
        * (lambda (...)
        *   <expression> ;= <car expression>
        *   )
        **********************************************************************/
        return compile(
                 car(expression),
                 lexical_environment,
                 continuation,
                 true // tail-call optimization
               );
      }
      else if (not car(expression))
      {
        /**********************************************************************
        * The expression may have following form.
        * If definition appears in <cdr expression>, it is an syntax error.
        *
        * (lambda (...)
        *   () ;= <car expression>
        *   <sequence> ;= <cdr expression>)
        **********************************************************************/
        return sequence(
                 cdr(expression),
                 lexical_environment,
                 continuation
               );
      }
      else if (not car(expression).is<pair>()
               or caar(expression) != intern("define"))
      {
        /**********************************************************************
        * The expression may have following form.
        *
        * (lambda (...)
        *   <non-definition expression> ;= <car expression>
        *   <sequence> ;= <cdr expression>)
        **********************************************************************/
        return compile(
                 car(expression), // <non-definition expression>
                 lexical_environment,
                 cons(
                   _pop_, // remove result of expression
                   sequence(
                     cdr(expression),
                     lexical_environment,
                     continuation
                   )
                 )
               );
      }
      else // 5.3.2 Internal Definitions
      {
        /**********************************************************************
         * The expression may have following form.
         * If definition appears in <car expression> or <cdr expression>, it is
         * an syntax error.
         *
         * (lambda (...)
         *   (define <variable> <initialization>) ;= <car expression>
         *   <sequence> ;= <cdr expression>)
         *********************************************************************/
        // std::cerr << "; letrec*\t; <expression> := " << expression << std::endl;

        // <bindings> := ( (<variable> <initialization>) ...)
        object bindings {list(
          cdar(expression) // (<variable> <initialization>)
        )};

        // <body> of letrec* := <sequence>+
        object body {};

        /**********************************************************************
        * Collect <definition>s from <cdr expression>.
        * It is guaranteed that <cdr expression> is not unit from first
        * conditional of this member function.
        *
        * The <cdr expression> may have following form.
        *
        * ( <expression 1>
        *   <expression 2>
        *   ...
        *   <expression N> )
        *
        **********************************************************************/
        for (iterator each {cdr(expression)}; each; ++each)
        {
          if (not car(each) or // unit (TODO? syntax-error)
              not car(each).is<pair>() or // <identifier or literal>
              caar(each) != intern("define"))
          {
            body = each;

            // std::cerr << "; letrec*\t; <bindings> := " << bindings << std::endl;
            //
            // std::cerr << "; letrec*\t; <body> := " << body << std::endl;
          }
          else
          {
            bindings = append(bindings, list(cdar(each)));
          }
        }

        const object formals {map(car, bindings)};
        // std::cerr << "; letrec*\t; <formals> := " << formals << std::endl;

        auto make_list = [&](auto size, const object& fill)
        {
          object result {};

          for (std::size_t k {0}; k < size; ++k)
          {
            result = cons(fill, result);
          }

          return result;
        };

        const object operands {make_list(length(formals), undefined)};
        // std::cerr << "; letrec*\t; <operands> := " << operands << std::endl;

        const object assignments {map(
          [this](auto&& each)
          {
            return intern("set!") | each;
          },
          bindings
        )};
        // std::cerr << "; letrec*\t; <assignments> := " << assignments << std::endl;

        const object result {cons(
          cons(
            intern("lambda"), formals, append(assignments, body)
          ),
          operands
        )};
        // std::cerr << "; letrec*\t; result := " << result << std::endl;

        return compile(
                 result,
                 lexical_environment,
                 continuation
               );
      }
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

    /**
     * <conditional> = (if <test> <consequent> <alternate>)
     **/
    object conditional(const object& expression,
                       const object& lexical_environment,
                       const object& continuation,
                       const bool optimization = false)
    {
      DEBUG_COMPILE(car(expression) << " ; => is <test>" << std::endl);

      if (optimization)
      {
        const auto consequent {compile(
          cadr(expression), lexical_environment, list(_return_), true
        )};

        const auto alternate {
          cddr(expression) ? compile(caddr(expression), lexical_environment, list(_return_), true)
                           : list(_load_literal_, undefined, _return_)
        };

        return compile(
                 car(expression), // <test>
                 lexical_environment,
                 cons(_select_tail_, consequent, alternate, cdr(continuation))
               );
      }
      else
      {
        const auto consequent {compile(cadr(expression), lexical_environment, list(_join_))};

        const auto alternate {
          cddr(expression) ? compile(caddr(expression), lexical_environment, list(_join_))
                           : list(_load_literal_, undefined, _join_)
        };

        return compile(
                 car(expression), // <test>
                 lexical_environment,
                 cons(_select_, consequent, alternate, continuation)
               );
      }
    }

    /**
     * <lambda expression> = (lambda <formals> <body>)
     **/
    object lambda(const object& expression,
                  const object& lexical_environment,
                  const object& continuation,
                  const bool = false)
    {
      DEBUG_COMPILE(car(expression) << " ; => is <formals>" << std::endl);

      return cons(
               _make_closure_,
               body(
                 cdr(expression), // <body>
                 cons(car(expression), lexical_environment), // extend lexical environment
                 list(_return_) // continuation of body (finally, must be return)
               ),
               continuation
             );
    }

    object call_cc(const object& expression,
                   const object& lexical_environment,
                   const object& continuation,
                   const bool = false)
    {
      DEBUG_COMPILE(car(expression) << " ; => is <procedure>" << std::endl);

      return cons(
               _make_continuation_,
               continuation,
               compile(
                 car(expression),
                 lexical_environment,
                 cons(_apply_, continuation)
               )
             );
    }

    object abstraction(const object& expression,
                       const object& lexical_environment,
                       const object& continuation, bool = false)
    {
      DEBUG_COMPILE(car(expression) << "\t; => <formals>" << std::endl);

      return cons(
               _make_environment_,
               program(
                 cdr(expression),
                 cons(car(expression), lexical_environment),
                 list(_return_)
               ),
               continuation
             );
    }

    object assignment(const object& expression,
                      const object& lexical_environment,
                      const object& continuation, bool = false)
    {
      DEBUG_COMPILE(car(expression) << " ; => is ");

      if (!expression)
      {
        throw syntax_error {"set!"};
      }
      else if (de_bruijn_index index {car(expression), lexical_environment}; index)
      {
        // XXX デバッグ用のトレースがないなら条件演算子でコンパクトにまとめたほうが良い
        if (index.is_variadic())
        {
          DEBUG_COMPILE_DECISION(" local variadic variable => " << list(_set_local_variadic_, index) << std::endl);

          return compile(
                   cadr(expression),
                   lexical_environment,
                   cons(_set_local_variadic_, index, continuation)
                 );
        }
        else
        {
          DEBUG_COMPILE_DECISION(" local variable => " << list(_set_local_, index) << std::endl);

          return compile(
                   cadr(expression),
                   lexical_environment,
                   cons(_set_local_, index, continuation)
                 );
        }
      }
      else
      {
        DEBUG_COMPILE_DECISION(" global variable => " << list(_set_global_, car(expression)) << std::endl);

        return compile(
                 cadr(expression),
                 lexical_environment,
                 cons(_set_global_, car(expression), continuation)
               );
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

