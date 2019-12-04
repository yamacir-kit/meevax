#ifndef INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
#define INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/special.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/symbol.hpp> // object::is<symbol>()

inline namespace ugly_macros
{
  static std::size_t depth {0};

  #define TRACE(N)                                                             \
  if (static_cast<SyntacticContinuation&>(*this).trace == true_object)         \
  {                                                                            \
    std::cerr << "; machine\t; \x1B[?7l" << take(c, N) << "\x1B[?7h" << std::endl; \
  }

  #define DEBUG_COMPILE(...)                                                   \
  if (   static_cast<SyntacticContinuation&>(*this).verbose          == true_object  \
      or static_cast<SyntacticContinuation&>(*this).verbose_compiler == true_object) \
  {                                                                            \
    std::cerr << (not depth ? "; compile\t; " : ";\t\t; ")                     \
              << std::string(depth * 2, ' ')                                   \
              << __VA_ARGS__;                                                  \
  }

  // TODO REMOVE THIS!!!
  #define DEBUG_COMPILE_SYNTAX(...)                                            \
  if (verbose == true_object or verbose_compiler == true_object)               \
  {                                                                            \
    std::cerr << (depth ? "; compile\t; " : ";\t\t; ")                         \
              << std::string(depth * 2, ' ')                                   \
              << __VA_ARGS__;                                                  \
  }

  #define DEBUG_COMPILE_DECISION(...)                                          \
  if (   static_cast<SyntacticContinuation&>(*this).verbose          == true_object  \
      or static_cast<SyntacticContinuation&>(*this).verbose_compiler == true_object) \
  {                                                                            \
    std::cerr << __VA_ARGS__ << attribute::normal << std::endl;                \
  }

  #define DEBUG_MACROEXPAND(...)                                               \
  if (   static_cast<SyntacticContinuation&>(*this).verbose          == true_object  \
      or static_cast<SyntacticContinuation&>(*this).verbose_compiler == true_object) \
  {                                                                            \
    std::cerr << "; macroexpand\t; "                                           \
              << std::string(depth * 2, ' ')                                   \
              << __VA_ARGS__;                                                  \
  }

  #define COMPILER_WARNING(...) \
  if (   static_cast<SyntacticContinuation&>(*this).verbose          == true_object  \
      or static_cast<SyntacticContinuation&>(*this).verbose_compiler == true_object) \
  {                                                                            \
    std::cerr << attribute::normal  << "; "                                    \
              << highlight::warning << "compiler"                              \
              << attribute::normal  << "\t; "                                  \
              << highlight::warning << __VA_ARGS__                             \
              << attribute::normal  << std::endl;                              \
  }

  #define NEST_IN  ++depth
  #define NEST_OUT --depth; DEBUG_COMPILE(")" << std::endl)

  // TODO REMOVE THIS!!!
  #define NEST_OUT_SYNTAX --depth; DEBUG_COMPILE_SYNTAX(")" << std::endl)
}

namespace meevax::kernel
{
  template <typename SyntacticContinuation>
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
      return static_cast<SyntacticContinuation&>(*this).interaction_environment();
    }

    template <typename... Ts>
    decltype(auto) intern(Ts&&... operands)
    {
      return
        static_cast<SyntacticContinuation&>(*this).intern(
          std::forward<decltype(operands)>(operands)...);
    }

    // TODO Remove
    // template <typename... Ts>
    // decltype(auto) export_(Ts&&... operands)
    // {
    //   return static_cast<SyntacticContinuation&>(*this).export_(std::forward<decltype(operands)>(operands)...);
    // }

  public:
    // Direct virtual machine instruction invocation.
    template <typename... Ts>
    decltype(auto) define(const object& key, Ts&&... operands)
    {
      // auto iter {export_(key, std::forward<decltype(operands)>(operands)...)};
      // interaction_environment().push(list(iter->first, iter->second));
      interaction_environment().push(
        list(key, std::forward<decltype(operands)>(operands)...));

      if (   static_cast<SyntacticContinuation&>(*this).verbose        == true_object
          or static_cast<SyntacticContinuation&>(*this).verbose_define == true_object)
      {
        std::cerr << "; define\t; " << caar(interaction_environment()) << "\r\x1b[40C\x1b[K " << cadar(interaction_environment()) << std::endl;
      }

      return interaction_environment(); // temporary
    }

    const object&
      lookup(
        const object& identifier,
        const object& environment)
    {
      if (not identifier or not environment)
      {
        return identifier;
      }
      else if (caar(environment) == identifier)
      {
        return cadar(environment);
      }
      else
      {
        return lookup(identifier, cdr(environment));
      }
    }

    /* ------------------------------------------------------------------------
    *
    * <expression> = <identifier>
    *              | <literal>
    *              | <procedure call>
    *              | <lambda expression>
    *              | <conditional>
    *              | <assignment>
    *              | <derived expression>
    *
    * TODO Change last boolean argument to template parameter (use if constexpr)
    *----------------------------------------------------------------------- */
    object compile(
      const object& expression,
      const object& lexical_environment = unit,
      const object& continuation = list(make<instruction>(mnemonic::STOP)),
      const bool optimizable = false)
    {
      if (not expression)
      {
        return
          cons(
            make<instruction>(mnemonic::LOAD_LITERAL), unit,
            continuation);
      }
      else if (not expression.is<pair>())
      {
        DEBUG_COMPILE(expression << highlight::comment << "\t; ");

        if (expression.is<symbol>()) // is variable
        {
          if (de_bruijn_index index {expression, lexical_environment}; index)
          {
            // XXX デバッグ用のトレースがないなら条件演算子でコンパクトにまとめたほうが良い
            if (index.is_variadic())
            {
              DEBUG_COMPILE_DECISION(
                "is <variable> references lexical variadic " << attribute::normal << index);

              return
                cons(
                  make<instruction>(mnemonic::LOAD_LOCAL_VARIADIC), index,
                  continuation);
            }
            else
            {
              DEBUG_COMPILE_DECISION(
                "is <variable> references lexical " << attribute::normal << index);

              return
                cons(
                  make<instruction>(mnemonic::LOAD_LOCAL), index,
                  continuation);
            }
          }
          else
          {
            DEBUG_COMPILE_DECISION(
              "is <variable> references dynamic value bound to the identifier");

            return
              cons(
                make<instruction>(mnemonic::LOAD_GLOBAL), expression,
                continuation);
          }
        }
        else
        {
          DEBUG_COMPILE_DECISION("is <self-evaluating>");

          return
            cons(
              make<instruction>(mnemonic::LOAD_LITERAL), expression,
              continuation);
        }
      }
      else // is (application . arguments)
      {
        if (object applicant {lookup(
              car(expression), interaction_environment()
            )};
            not applicant)
        {
          COMPILER_WARNING(
            "compiler detected application of variable currently bounds "
            "empty-list. if the variable will not reset with applicable object "
            "later, cause runtime error.");
        }
        else if (applicant.is<special>()
                 and not de_bruijn_index(car(expression), lexical_environment))
        {
          DEBUG_COMPILE(
            "(" << car(expression)
                << highlight::comment << "\t; is <primitive expression> "
                << attribute::normal << applicant << std::endl);

          NEST_IN;
          auto result {std::invoke(applicant.as<special>(),
            cdr(expression), lexical_environment, continuation, optimizable
          )};
          NEST_OUT;

          return result;
        }
        else if (applicant.is<SyntacticContinuation>()
                 and not de_bruijn_index(car(expression), lexical_environment))
        {
          DEBUG_COMPILE(
            "(" << car(expression)
                << highlight::comment << "\t; is <macro use> of <derived expression> "
                << attribute::normal << applicant
                << attribute::normal << std::endl);

          // std::cerr << "Syntactic-Continuation holds "
          //           << applicant.as<SyntacticContinuation>().continuation()
          //           << std::endl;

          const auto expanded {
            applicant.as<SyntacticContinuation>().expand(
              expression)
          };

          DEBUG_MACROEXPAND(expanded << std::endl);

          NEST_IN;
          auto result {compile(expanded, lexical_environment, continuation)};
          NEST_OUT;

          return result;
        }

        DEBUG_COMPILE(
          "(" << highlight::comment << "\t; is <procedure call>"
              << attribute::normal << std::endl);

        NEST_IN;
        auto result {
          operand(
            cdr(expression),
            lexical_environment,
            compile(
              car(expression),
              lexical_environment,
              cons(
                make<instruction>(
                  optimizable ? mnemonic::APPLY_TAIL : mnemonic::APPLY),
                continuation)))
        };
        NEST_OUT;
        return result;
      }
    }

    decltype(auto) execute(const object& expression)
    {
      c = expression;

      if (   static_cast<SyntacticContinuation&>(*this).verbose         == true_object
          or static_cast<SyntacticContinuation&>(*this).verbose_machine == true_object)
      {
        std::cerr << "; machine\t; " << c << std::endl;
      }

      return execute();
    }

    object execute()
    {
    dispatch:
      switch (c.top().template as<instruction>().code)
      {
      case mnemonic::LOAD_LOCAL: // S E (LOAD_LOCAL (i . j) . C) D => (value . S) E C D
        TRACE(2);
        {
          homoiconic_iterator region {e};
          std::advance(region, int {caadr(c).template as<real>()});

          homoiconic_iterator position {*region};
          std::advance(position, int {cdadr(c).template as<real>()});

          s.push(*position);
        }
        c.pop(2);
        goto dispatch;

      case mnemonic::LOAD_LOCAL_VARIADIC:
        TRACE(2);
        {
          homoiconic_iterator region {e};
          std::advance(region, int {caadr(c).template as<real>()});

          homoiconic_iterator position {*region};
          std::advance(position, int {cdadr(c).template as<real>()});

          s.push(position);
        }
        c.pop(2);
        goto dispatch;

      case mnemonic::LOAD_LITERAL: // S E (LOAD_LITERAL constant . C) D => (constant . S) E C D
        TRACE(2);
        s.push(cadr(c));
        c.pop(2);
        goto dispatch;

      case mnemonic::LOAD_GLOBAL: // S E (LOAD_GLOBAL symbol . C) D => (value . S) E C D
        TRACE(2);
        if (auto value {
              assoc(
                cadr(c),
                interaction_environment())
            }; value != unbound)
        {
          s.push(value);
        }
        else
        {
          // throw evaluation_error {cadr(c), " is unbound"};

          if (   static_cast<SyntacticContinuation&>(*this).verbose == true_object
              or static_cast<SyntacticContinuation&>(*this).verbose_machine == true_object)
          {
            std::cerr << "; machine\t; instruction " << car(c) << " received undefined variable " << cadr(c) << ".\n"
                      << ";\t\t; start implicit renaming..." << std::endl;
          }

          /* ------------------------------------------------------------------
          * When an undefined symbol is evaluated, it returns a symbol that is
          * guaranteed not to collide with any symbol from the past to the
          * future. This behavior is defined for the hygienic-macro.
          *----------------------------------------------------------------- */
          s.push(static_cast<SyntacticContinuation&>(*this).rename(cadr(c)));
        }
        c.pop(2);
        goto dispatch;

      case mnemonic::FORK: // S E (FORK code . C) => (subprogram . S) E C D
        TRACE(2);
        s.push(
          make<SyntacticContinuation>(
            make<closure>(cadr(c), e),
            interaction_environment()));
        c.pop(2);
        goto dispatch;

      case mnemonic::MAKE_CLOSURE: // S E (MAKE_CLOSURE code . C) => (closure . S) E C D
        TRACE(2);
        {
          s.push(
            make<closure>(cadr(c), e));
          c.pop(2);
        }
        goto dispatch;

      case mnemonic::MAKE_CONTINUATION: // S E (MAKE_CONTINUATION code . C) D => ((continuation) . S) E C D
        TRACE(2);
        s.push(
          list(
            make<continuation>(s, cons(e, cadr(c), d)))); // XXX 本当は cons(s, e, cadr(c), d) としたいけど、make<continuation> の引数はペア型の引数である必要があるため歪な形になってる。
        c.pop(2);
        goto dispatch;

      /* ====*/ case mnemonic::MAKE_SYNTACTIC_CONTINUATION: /*=================
      *
      */ TRACE(2);                                                           /*
      *
      *                S  E (MAKE_SC code . C) D
      *
      *  => (program . S) E                 C  D
      *
      *====================================================================== */
        // s = cons(
        //       make<SyntacticContinuation>(
        //         cadr(c),
        //         interaction_environment()),
        //       s);
        // c = cddr(c);
        s = make<SyntacticContinuation>(
              car(s),
              interaction_environment())
          | cdr(s);
        c.pop(1);
        goto dispatch;

      case mnemonic::SELECT: // (boolean . S) E (SELECT then else . C) D => S E then/else (C . D)
        TRACE(3);
        d.push(cdddr(c));
        c = car(s) != false_object ? cadr(c) : caddr(c);
        s.pop(1);
        goto dispatch;

      case mnemonic::SELECT_TAIL:
        TRACE(3);
        c = car(s) != false_object ? cadr(c) : caddr(c);
        s.pop(1);
        goto dispatch;

      case mnemonic::JOIN: // S E (JOIN . x) (C . D) => S E C D
        TRACE(1);
        c = car(d);
        d.pop(1);
        goto dispatch;

      case mnemonic::DEFINE:
        TRACE(2);
        define(cadr(c), car(s));
        car(s) = cadr(c); // return value of define
        c.pop(2);
        goto dispatch;

      case mnemonic::APPLY:
        TRACE(1);

        if (const object callee {car(s)}; not callee)
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
        else if (callee.is<procedure>()) // (procedure operands . S) E (APPLY . C) D => (result . S) E C D
        {
          s = std::invoke(callee.as<procedure>(), resource {}, cadr(s))
            | cddr(s);
          c.pop(1);
        }
        // else if (callee.is<SyntacticContinuation>())
        // {
        //   s = callee.as<SyntacticContinuation>().expand(car(s) | cadr(s)) | cddr(s);
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

      case mnemonic::APPLY_TAIL:
        TRACE(1);

        if (object callee {car(s)}; not callee)
        {
          throw evaluation_error {"unit is not appliciable"};
        }
        else if (callee.is<closure>()) // (closure operands . S) E (APPLY . C) D
        {
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure operands . S) E (APPLY . C) D => (result . S) E C D
        {
          s = std::invoke(callee.as<procedure>(), resource {}, cadr(s))
            | cddr(s);
          c.pop(1);
        }
        // else if (callee.is<SyntacticContinuation>())
        // {
        //   s = callee.as<SyntacticContinuation>().expand(car(s) | cadr(s)) | cddr(s);
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

      case mnemonic::RETURN: // (value . S) E (RETURN . C) (S' E' C' . D) => (value . S') E' C' D
        TRACE(1);
        s = cons(car(s), d.pop());
        e = d.pop();
        c = d.pop();
        goto dispatch;

      /* ====*/ case mnemonic::PUSH: /*========================================
      *
      */ TRACE(1);                                                           /*
      *
      *     ( X   Y  . S) E (PUSH . C) D
      *
      *  => ((X . Y) . S) E         C  D
      *
      *====================================================================== */
        s = cons(cons(car(s), cadr(s)), cddr(s));
        c.pop(1);
        goto dispatch;

      case mnemonic::POP: // (var . S) E (POP . C) D => S E C D
        TRACE(1);
        s.pop(1);
        c.pop(1);
        goto dispatch;

      case mnemonic::SET_GLOBAL: // (value . S) E (SET_GLOBAL symbol . C) D => (value . S) E C D
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

      case mnemonic::SET_LOCAL: // (value . S) E (SET_LOCAL (i . j) . C) D => (value . S) E C D
        TRACE(2);
        {
          homoiconic_iterator region {e};
          std::advance(region, int {caadr(c).template as<real>()});

          homoiconic_iterator position {*region};
          std::advance(position, int {cdadr(c).template as<real>()});

          std::atomic_store(&car(position), car(s));
        }
        c.pop(2);
        goto dispatch;

      case mnemonic::SET_LOCAL_VARIADIC:
        TRACE(2);
        {
          homoiconic_iterator region {e};
          std::advance(region, int {caadr(c).template as<real>()});

          homoiconic_iterator position {*region};
          std::advance(position, int {cdadr(c).template as<real>()} - 1);

          std::atomic_store(&cdr(position), car(s));
        }
        c.pop(2);
        goto dispatch;

      case mnemonic::STOP: // (result . S) E (STOP . C) D
      default:
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

          for (homoiconic_iterator position {region}; position; ++position)
          {
            if (position.is<pair>() && *position == variable)
            {
              variadic = false;

              return
                cons(
                  make<real>(i),
                  make<real>(j));
            }
            else if (not position.is<pair>() && position == variable)
            {
              variadic = true;

              return
                cons(
                  make<real>(i),
                  make<real>(j));
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

  protected:
    /* ==== Quotation =========================================================
    *
    * <quotation> = (quote <datum>)
    *
    *======================================================================== */
    const object
      quotation(
        const object& expression,
        const object&,
        const object& continuation,
        const bool = false)
    {
      DEBUG_COMPILE(
        car(expression) << highlight::comment << "\t; is <datum>"
                        << attribute::normal << std::endl);
      return
        cons(
          make<instruction>(mnemonic::LOAD_LITERAL), car(expression),
          continuation);
    }

    /* ==== Sequence ==========================================================
    *
    * <sequence> = <command>* <expression>
    *
    * <command> = <expression>
    *
    *======================================================================== */
    const object
      sequence(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool optimization = false)
    {
      if (not cdr(expression)) // is tail sequence
      {
        return
          compile(
            car(expression),
            lexical_environment,
            continuation,
            optimization);
      }
      else
      {
        return
          compile(
            car(expression), // head expression
            lexical_environment,
            cons(
              make<instruction>(mnemonic::POP), // pop result of head expression
              sequence(
                cdr(expression), // rest expressions
                lexical_environment,
                continuation)));
      }
    }

    /* ==== Definition ========================================================
    *
    * <definition> = (define <identifier> <expression>)
    *
    *======================================================================== */
    const object
      definition(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool = false)
    {
      if (not lexical_environment)
      {
        DEBUG_COMPILE(
          car(expression) << highlight::comment << "\t; is <variable>"
                          << attribute::normal << std::endl);
        return
          compile(
            cdr(expression) ? cadr(expression) : undefined,
            lexical_environment,
            cons(
              make<instruction>(mnemonic::DEFINE), car(expression),
              continuation));
      }
      else
      {
        // throw syntax_error_about_internal_define {
        //   "definition cannot appear in this context"
        // };
        throw
          compile(
            cdr(expression) ? cadr(expression) : undefined,
            lexical_environment,
            cons(
              make<instruction>(mnemonic::DEFINE), car(expression),
              continuation));
      }
    }

    /* ==== Lambda Body =======================================================
    *
    * <body> = <definition>* <sequence>
    *
    *======================================================================== */
    const object
      body(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool subprogram_declaration = false)
    {
      /* ----------------------------------------------------------------------
      *
      * The expression may have following form.
      *
      * (lambda (...)
      *   <definition or command>  ;= <car expression>
      *   <definition or sequence> ;= <cdr expression>
      *   )
      *
      *---------------------------------------------------------------------- */
      if (not cdr(expression)) // is tail sequence
      {
        /* --------------------------------------------------------------------
        *
        * The expression may have following form.
        * If definition appears in <car expression>, it is an syntax error.
        *
        * (lambda (...)
        *   <expression> ;= <car expression>
        *   )
        *
        *-------------------------------------------------------------------- */
        try
        {
          return
            compile(
              car(expression),
              lexical_environment,
              continuation,
              true); // tail-call optimization
        }
        catch (const object& definition)
        {
          // if (subprogram_declaration)
          // {
          //   return definition;
          // }
          // else
          // {
            throw syntax_error_about_internal_define {
              "definition cannot appear in this context"
            };
          // }
        }
      }
      else if (not car(expression))
      {
        /* --------------------------------------------------------------------
        *
        * The expression may have following form.
        * If definition appears in <cdr expression>, it is an syntax error.
        *
        * (lambda (...)
        *   ()         ;= <car expression>
        *   <sequence> ;= <cdr expression>)
        *
        *-------------------------------------------------------------------- */
        // if (subprogram_declaration)
        // {
        //   return
        //     body(
        //       cdr(expression),
        //       lexical_environment,
        //       continuation,
        //       true);
        // }
        // else
        // {
        try
        {
          return
            sequence(
              cdr(expression),
              lexical_environment,
              continuation);
        }
        catch (const object& definition)
        {
          throw syntax_error_about_internal_define {
            "definition cannot appear in this context"
          };
        }
        // }
      }
      else if (not car(expression).is<pair>()
               or caar(expression) != intern("define")) // XXX THIS IS NOT HYGIENIC
      {
        /* --------------------------------------------------------------------
        *
        * The expression may have following form.
        *
        * (lambda (...)
        *   <non-definition expression> ;= <car expression>
        *   <sequence>                  ;= <cdr expression>)
        *
        *-------------------------------------------------------------------- */
        // if (subprogram_declaration)
        // {
        //   const auto subprogram_continuation {
        //     cons(
        //       make<instruction>(mnemonic::POP),
        //       body(
        //         cdr(expression),
        //         lexical_environment,
        //         continuation,
        //         true))
        //   };
        //
        //   try
        //   {
        //     return
        //       compile(
        //         car(expression),
        //         lexical_environment,
        //         subprogram_continuation);
        //   }
        //   catch (const object& definition)
        //   {
        //     return
        //       cons(
        //         definition,
        //         subprogram_continuation);
        //   }
        // }
        // else
        // {
        return
          compile(
            car(expression), // <non-definition expression>
            lexical_environment,
            cons(
              make<instruction>(mnemonic::POP), // remove result of expression
              sequence(
                cdr(expression),
                lexical_environment,
                continuation)));
        // }
      }
      else // 5.3.2 Internal Definitions
      {
        /* --------------------------------------------------------------------
        *
        * The expression may have following form.
        * If definition appears in <car expression> or <cdr expression>, it is
        * an syntax error.
        *
        * (lambda (...)
        *   (define <variable> <initialization>) ;= <car expression>
        *   <sequence> ;= <cdr expression>)
        *
        *-------------------------------------------------------------------- */
        // std::cerr << "; letrec*\t; <expression> := " << expression << std::endl;

        // <bindings> := ( (<variable> <initialization>) ...)
        object bindings {list(
          cdar(expression) // (<variable> <initialization>)
        )};

        // <body> of letrec* := <sequence>+
        object body {};

        /* --------------------------------------------------------------------
        *
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
        *-------------------------------------------------------------------- */
        for (homoiconic_iterator each {cdr(expression)}; each; ++each)
        {
          if (not car(each) or // unit (TODO? syntax-error)
              not car(each).is<pair>() or // <identifier or literal>
              caar(each) != intern("define")) // XXX THIS IS NOT HYGIENIC
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

        const object result {
          cons(
            cons(
              intern("lambda"),
              formals,
              append(assignments, body)),
            operands)
        };
        // std::cerr << "; letrec*\t; result := " << result << std::endl;

        return
          compile(
            result,
            lexical_environment,
            continuation);
      }
    }

    /* ==== Operand ===========================================================
    *
    * <operand> = <expression>
    *
    *======================================================================== */
    const object
      operand(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool = false)
    {
      if (expression and expression.is<pair>())
      {
        return
          operand(
            cdr(expression),
            lexical_environment,
            compile(
              car(expression),
              lexical_environment,
              cons(
                make<instruction>(mnemonic::PUSH),
                continuation)));
      }
      else
      {
        return
          compile(
            expression,
            lexical_environment,
            continuation);
      }
    }

    /* ==== Conditional =======================================================
    *
    * <conditional> = (if <test> <consequent> <alternate>)
    *
    *======================================================================== */
    const object
      conditional(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool optimization = false)
    {
      DEBUG_COMPILE(
        car(expression) << highlight::comment << "\t; is <test>"
                        << attribute::normal << std::endl);

      if (optimization)
      {
        const auto consequent {
          compile(
            cadr(expression),
            lexical_environment,
            list(
              make<instruction>(mnemonic::RETURN)),
            true)
        };

        const auto alternate {
          cddr(expression)
            ? compile(
                caddr(expression),
                lexical_environment,
                list(
                  make<instruction>(mnemonic::RETURN)),
                true)
            : list(
                make<instruction>(mnemonic::LOAD_LITERAL), undefined,
                make<instruction>(mnemonic::RETURN))
        };

        return
          compile(
            car(expression), // <test>
            lexical_environment,
            cons(
              make<instruction>(mnemonic::SELECT_TAIL),
              consequent,
              alternate,
              cdr(continuation)));
      }
      else
      {
        const auto consequent {
          compile(
            cadr(expression),
            lexical_environment,
            list(make<instruction>(mnemonic::JOIN)))
        };

        const auto alternate {
          cddr(expression)
            ? compile(
                caddr(expression),
                lexical_environment,
                list(make<instruction>(mnemonic::JOIN)))
            : list(
                make<instruction>(mnemonic::LOAD_LITERAL), undefined,
                make<instruction>(mnemonic::JOIN))
        };

        return
          compile(
            car(expression), // <test>
            lexical_environment,
            cons(
              make<instruction>(mnemonic::SELECT), consequent, alternate,
              continuation));
      }
    }

    /* ==== Lambda Expression =================================================
    *
    * <lambda expression> = (lambda <formals> <body>)
    *
    *======================================================================== */
    const object
      lambda(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool = false)
    {
      DEBUG_COMPILE(
        car(expression) << highlight::comment << "\t; is <formals>"
                        << attribute::normal << std::endl);

      return
        cons(
          make<instruction>(mnemonic::MAKE_CLOSURE),
          body(
            cdr(expression), // <body>
            cons(
              car(expression),
              lexical_environment), // extend lexical environment
            list(
              make<instruction>(mnemonic::RETURN))), // continuation of body (finally, must be return)
          continuation);
    }

    /* ==== Call-With-Current-Continuation ====================================
    *
    * TODO documentation
    *
    *======================================================================== */
    const object
      call_cc(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool = false)
    {
      DEBUG_COMPILE(
        car(expression) << highlight::comment << "\t; is <procedure>"
                        << attribute::normal << std::endl);
      return
        cons(
          make<instruction>(mnemonic::MAKE_CONTINUATION),
          continuation,
          compile(
            car(expression),
            lexical_environment,
            cons(
              make<instruction>(mnemonic::APPLY),
              continuation)));
    }

    /* ==== Program ===========================================================
    *
    * <program> = <import declaration>+
    *             <definition or command>+
    *             <definition or expression>+
    *
    * <definition or command> = <definition>
    *                         | <command>
    *                         | (begin <command or definition>+)
    *
    * <command> = <an expression whose return value will be ignored>
    *
    *
    * <library> = (define-library <library name> <library declaration>+)
    *
    * <library name> = (<library name part>+)
    *
    * <library name part> = <identifier>
    *                     | <unsigned integer 10>
    *
    * <library declaration>
    *
    *======================================================================== */
    const object
      program(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool = false)
    {
      if (not cdr(expression)) // is tail sequence
      {
        try
        {
          return
            compile(
              car(expression),
              lexical_environment,
              continuation); // TODO tail-call-optimizable?
        }
        catch (const object& definition)
        {
          return definition;
        }
      }
      else
      {
        const auto declarations {
          program(
            cdr(expression),
            lexical_environment,
            continuation)
        };

        try
        {
          return
            compile(
              car(expression),
              lexical_environment,
              cons(
                make<instruction>(mnemonic::POP), // remove result of expression
                declarations));
        }
        catch (const object& definition)
        {
          return
            cons(
              definition,
              make<instruction>(mnemonic::POP),
              declarations);
        }
      }
    }

    /* ==== Call-With-Current-Syntactic-Continuation ==========================
    *
    * TODO documentation
    *
    *======================================================================== */
    const object
      call_csc(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool = false)
    {
      DEBUG_COMPILE(
        car(expression) << highlight::comment << "\t; is <subprogram>"
                        << attribute::normal << std::endl);
      // return
      //   cons(
      //     make<instruction>(mnemonic::MAKE_SYNTACTIC_CONTINUATION),
      //     program(
      //       expression,
      //       lexical_environment,
      //       list(
      //         make<instruction>(mnemonic::RETURN))),
      //     continuation);

      return
        compile(
          car(expression),
          lexical_environment,
          cons(
            make<instruction>(mnemonic::MAKE_SYNTACTIC_CONTINUATION),
            continuation),
          true); // for subprogram_declaration
    }

    /* ==== Fork ==============================================================
    *
    * TODO documentation
    *
    *======================================================================== */
    const object
      fork(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool = false)
    {
      DEBUG_COMPILE(
        car(expression) << highlight::comment << "\t; is <subprogram parameters>"
                        << attribute::normal << std::endl);
      return
        cons(
          make<instruction>(mnemonic::FORK),
          program(
            cdr(expression),
            cons(car(expression), lexical_environment),
            list(make<instruction>(mnemonic::RETURN))),
          continuation);
    }

    /* ==== Assignment ========================================================
    *
    * TODO documentation
    *
    *======================================================================== */
    const object
      assignment(
        const object& expression,
        const object& lexical_environment,
        const object& continuation,
        const bool = false)
    {
      DEBUG_COMPILE(car(expression) << highlight::comment << "\t; is ");

      if (!expression)
      {
        throw syntax_error {"set!"};
      }
      else if (de_bruijn_index index {
                 car(expression), lexical_environment
               }; index)
      {
        if (index.is_variadic())
        {
          DEBUG_COMPILE_DECISION(
            "<identifier> of lexical variadic " << attribute::normal << index);

          return
            compile(
              cadr(expression),
              lexical_environment,
              cons(
                make<instruction>(mnemonic::SET_LOCAL_VARIADIC), index,
                continuation));
        }
        else
        {
          DEBUG_COMPILE_DECISION("<identifier> of lexical " << attribute::normal << index);

          return
            compile(
              cadr(expression),
              lexical_environment,
              cons(
                make<instruction>(mnemonic::SET_LOCAL), index,
                continuation));
        }
      }
      else
      {
        DEBUG_COMPILE_DECISION("<identifier> of dynamic variable" << attribute::normal);

        return
          compile(
            cadr(expression),
            lexical_environment,
            cons(
              make<instruction>(mnemonic::SET_GLOBAL),
              car(expression),
              continuation));
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

