#ifndef INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
#define INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/de_brujin_index.hpp>
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
  if (const auto& config {static_cast<SyntacticContinuation&>(*this)};         \
      config.trace == true_object)                                             \
  {                                                                            \
    std::cerr << "; machine\t; \x1B[?7l"                                       \
              << take(c, N)                                                    \
              <<              "\x1B[?7h"                                       \
              << std::endl;                                                    \
  }

  #define IF_VERBOSE_COMPILER()                                                \
  if (const auto& config {static_cast<SyntacticContinuation&>(*this)};         \
         config.verbose          == true_object                                \
      or config.verbose_compiler == true_object)

  #define DEBUG_COMPILE(...)                                                   \
  IF_VERBOSE_COMPILER()                                                        \
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
  IF_VERBOSE_COMPILER()                                                        \
  {                                                                            \
    std::cerr << __VA_ARGS__ << attribute::normal << std::endl;                \
  }

  #define DEBUG_MACROEXPAND(...)                                               \
  IF_VERBOSE_COMPILER()                                                        \
  {                                                                            \
    std::cerr << "; macroexpand\t; "                                           \
              << std::string(depth * 2, ' ')                                   \
              << __VA_ARGS__;                                                  \
  }

  #define COMPILER_WARNING(...) \
  IF_VERBOSE_COMPILER()                                                        \
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
    object s, // main stack
           e, // lexical environment
           c, // control stack
           d; // dump stack (current-continuation)

  private: // CRTP
    #define CRTP(IDENTIFIER)                                                   \
    template <typename... Ts>                                                  \
    decltype(auto) IDENTIFIER(Ts&&... operands)                                \
    {                                                                          \
      return                                                                   \
        static_cast<SyntacticContinuation&>(*this).IDENTIFIER(                 \
          std::forward<decltype(operands)>(operands)...);                      \
    }

    CRTP(change)
    CRTP(interaction_environment)
    CRTP(intern)
    CRTP(rename)

    #undef CRTP

  public:
    // Direct virtual machine instruction invocation.
    template <typename... Ts>
    decltype(auto) define(const object& identifier, Ts&&... operands)
    {
      push(
        interaction_environment(),
        list(
          identifier,
          change(
            identifier,
            std::forward<decltype(operands)>(operands)...)));

      if (const auto& config {static_cast<SyntacticContinuation&>(*this)};
             config.verbose        == true_object
          or config.verbose_define == true_object)
      {
        std::cerr << "; define\t; "
                  << caar(interaction_environment())
                  << "\r\x1b[40C\x1b[K "
                  << cadar(interaction_environment())
                  << std::endl;
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
        return
          lookup(
            identifier,
            cdr(environment));
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
    *----------------------------------------------------------------------- */
    object compile(
      const object& expression,
      const object& frames = unit,
      const object& continuation = list(make<instruction>(mnemonic::STOP)),
      const compilation_context in_a = as_is)
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
          if (de_bruijn_index index {expression, frames}; index)
          {
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
                 and not de_bruijn_index(car(expression), frames))
        {
          DEBUG_COMPILE(
            "(" << car(expression)
                << highlight::comment << "\t; is <primitive expression> "
                << attribute::normal << applicant << std::endl);

          NEST_IN;
          auto result {std::invoke(applicant.as<special>(),
            cdr(expression), frames, continuation, in_a
          )};
          NEST_OUT;

          return result;
        }
        else if (applicant.is<SyntacticContinuation>()
                 and not de_bruijn_index(car(expression), frames))
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
          auto result {compile(expanded, frames, continuation)};
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
            frames,
            compile(
              car(expression),
              frames,
              cons(
                make<instruction>(
                  in_a.tail_expression ? mnemonic::APPLY_TAIL
                                       : mnemonic::APPLY),
                continuation)))
        };
        NEST_OUT;
        return result;
      }
    }

    // XXX DO NOT USE THIS EXCEPT FOR THE EVALUATE PROCEDURE.
    decltype(auto) execute_interrupt(const object& expression)
    {
      push(
        d,
        s,
        e,
        cons(
          make<instruction>(mnemonic::STOP),
          c ? cdr(c) : c));

      s = unit;
      e = unit;
      c = expression;

      if (   static_cast<SyntacticContinuation&>(*this).verbose         == true_object
          or static_cast<SyntacticContinuation&>(*this).verbose_machine == true_object)
      {
        std::cerr << "; machine\t; " << c << std::endl;
      }

      const auto result {execute()};

      s = pop(d);
      e = pop(d);
      c = pop(d);

      return result;
    }

    object execute() // try
    {
    dispatch:
      switch (car(c).template as<instruction>().code)
      {
      case mnemonic::LOAD_LOCAL: // S E (LOAD_LOCAL (i . j) . C) D => (value . S) E C D
        TRACE(2);
        {
          homoiconic_iterator region {e};
          std::advance(region, int {caadr(c).template as<real>()});

          homoiconic_iterator position {*region};
          std::advance(position, int {cdadr(c).template as<real>()});

          push(s, *position);
        }
        pop<2>(c);
        goto dispatch;

      case mnemonic::LOAD_LOCAL_VARIADIC:
        TRACE(2);
        {
          homoiconic_iterator region {e};
          std::advance(region, int {caadr(c).template as<real>()});

          homoiconic_iterator position {*region};
          std::advance(position, int {cdadr(c).template as<real>()});

          push(s, position);
        }
        pop<2>(c);
        goto dispatch;

      case mnemonic::LOAD_LITERAL: // S E (LOAD_LITERAL constant . C) D => (constant . S) E C D
        TRACE(2);
        push(s, cadr(c));
        pop<2>(c);
        goto dispatch;

      case mnemonic::LOAD_GLOBAL: // S E (LOAD_GLOBAL symbol . C) D => (value . S) E C D
        TRACE(2);
        if (auto value {
              assoc( // XXX assq?
                cadr(c),
                interaction_environment())
            }; value != unbound)
        {
          push(s, value);
        }
        else
        {
          // throw evaluation_error {cadr(c), " is unbound"};

          if (   static_cast<SyntacticContinuation&>(*this).verbose == true_object
              or static_cast<SyntacticContinuation&>(*this).verbose_machine == true_object)
          {
            std::cerr << "; machine\t; instruction "
                      << car(c)
                      << " received undefined variable "
                      << cadr(c)
                      << std::endl;
          }

          /* ------------------------------------------------------------------
          * When an undefined symbol is evaluated, it returns a symbol that is
          * guaranteed not to collide with any symbol from the past to the
          * future. This behavior is defined for the hygienic-macro.
          *----------------------------------------------------------------- */
          push(s, rename(cadr(c)));
        }
        pop<2>(c);
        goto dispatch;

      case mnemonic::FORK: // S E (FORK code . C) => (subprogram . S) E C D
        TRACE(2);
        push(
          s,
          make<SyntacticContinuation>(
            make<closure>(cadr(c), e),
            interaction_environment()));
        pop<2>(c);
        goto dispatch;

      case mnemonic::MAKE_CLOSURE: // S E (MAKE_CLOSURE code . C) => (closure . S) E C D
        TRACE(2);
        {
          push(s, make<closure>(cadr(c), e));
          pop<2>(c);
        }
        goto dispatch;

      case mnemonic::MAKE_CONTINUATION: // S E (MAKE_CONTINUATION code . C) D => ((continuation) . S) E C D
        TRACE(2);
        push(
          s,
          list(
            make<continuation>(
              s,
              cons(
                e,
                cadr(c),
                d))));
        pop<2>(c);
        goto dispatch;

      /* ====*/ case mnemonic::MAKE_SYNTACTIC_CONTINUATION: /*=================
      *
      */ TRACE(2);                                                            /*
      *
      *     (closure . S) E (MAKE_SC . C) D
      *
      *  => (program . S) E            C  D
      *
      *====================================================================== */
        push(
          s,
          make<SyntacticContinuation>(
            pop(s), // XXX car(s)?
            interaction_environment()));
        pop<1>(c);
        goto dispatch;

      case mnemonic::SELECT: // (boolean . S) E (SELECT then else . C) D => S E then/else (C . D)
        TRACE(3);
        push(d, cdddr(c));
        c = car(s) != false_object ? cadr(c) : caddr(c);
        pop<1>(s);
        goto dispatch;

      case mnemonic::SELECT_TAIL:
        TRACE(3);
        c = car(s) != false_object ? cadr(c) : caddr(c);
        pop<1>(s);
        goto dispatch;

      case mnemonic::JOIN: // S E (JOIN . x) (C . D) => S E C D
        TRACE(1);
        c = car(d);
        pop<1>(d);
        goto dispatch;

      case mnemonic::DEFINE:
        TRACE(2);
        define(cadr(c), car(s));
        car(s) = cadr(c); // return value of define
        pop<2>(c);
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
          push(d, cddr(s), e, cdr(c));
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure operands . S) E (APPLY . C) D => (result . S) E C D
        {
          s = cons(
                std::invoke(
                  callee.as<procedure>(),
                  resource {},
                  cadr(s)),
                cddr(s));
          pop<1>(c);
        }
        // else if (callee.is<SyntacticContinuation>())
        // {
        //   s = cons(
        //         callee.as<SyntacticContinuation>().expand(
        //           cons(
        //             car(s),
        //             cadr(s))),
        //         cddr(s));
        //   pop<1>(c);
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
          pop<1>(c);
        }
        // else if (callee.is<SyntacticContinuation>())
        // {
        //   s = cons(
        //         callee.as<SyntacticContinuation>().expand(
        //           cons(
        //             car(s),
        //             cadr(s))),
        //         cddr(s));
        //   pop<1>(c);
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

      /* ====*/ case mnemonic::RETURN: /*======================================
      *
      */ TRACE(1); /*
      *
      *    (result . S) E (RETURN . C) (s e c . D)
      *
      * => (result . s) e           c           D
      *
      *====================================================================== */
        s = cons(
              car(s), // The result of procedure
              pop(d));
        e = pop(d);
        c = pop(d);
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
        pop<1>(c);
        goto dispatch;

      case mnemonic::POP: // (var . S) E (POP . C) D => S E C D
        TRACE(1);
        pop<1>(s);
        pop<1>(c);
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
        pop<2>(c);
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
        pop<2>(c);
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
        pop<2>(c);
        goto dispatch;

      case mnemonic::STOP: // (result . S) E (STOP . C) D
      default:
        TRACE(1);
        pop<1>(c);
        return pop(s); // car(s);
      }
    }
    // catch (const kernel_error_about_pair& error)
    // {
    //   std::cerr << "; machine\t; internal-error occured" << std::endl;
    //
    //   std::cerr << ";\t\t; main stack" << std::endl;
    //
    //   for (const auto& formal : s)
    //   {
    //     std::cerr << ";\t\t;   (";
    //
    //     for (const auto& value : formal)
    //     {
    //       std::cerr << value << std::endl << ";\t\t;    ";
    //     }
    //
    //     std::cerr << ")" << std::endl;
    //   }
    //
    //   std::cerr << ";\t\t; environment stack " << std::endl;
    //
    //   for (const auto& each : e)
    //   {
    //     std::cerr << ";\t\t;   " << each << std::endl;
    //   }
    //
    //   std::cerr << ";\t\t; control stack" << std::endl;
    //
    //   for (const auto& each : c)
    //   {
    //     std::cerr << ";\t\t;   " << each << std::endl;
    //   }
    //
    //   std::cerr << ";\t\t; dump stack" << std::endl;
    //
    //   for (const auto& each : d)
    //   {
    //     std::cerr << ";\t\t;   " << each << std::endl;
    //   }
    //
    //   std::cerr << "; system\t; exit" << std::endl;
    //   std::exit(boost::exit_exception_failure);
    // }

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
        const compilation_context = as_is)
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
        const object& frames,
        const object& continuation,
        const compilation_context in_a = as_is)
    {
      if (in_a.program_declaration)
      {
        // std::cerr << "COMPILING SEQUENCE IN A PROGRAM DECLARATION"
        //           << std::endl;

        if (not cdr(expression))
        {
          return
            compile(
              car(expression),
              frames,
              continuation,
              // as_tail_expression_of_program_declaration
              as_program_declaration);
        }
        else
        {
          return
            compile(
              car(expression),
              frames,
              cons(
                make<instruction>(mnemonic::POP),
                sequence(
                  cdr(expression),
                  frames,
                  continuation,
                  as_program_declaration)),
              as_program_declaration);
        }
      }
      else
      {
        if (not cdr(expression)) // is tail sequence
        {
          return
            compile(
              car(expression),
              frames,
              continuation,
              as_tail_expression);
        }
        else
        {
          return
            compile(
              car(expression), // head expression
              frames,
              cons(
                make<instruction>(mnemonic::POP), // pop result of head expression
                sequence(
                  cdr(expression), // rest expressions
                  frames,
                  continuation)));
        }
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
        const object& frames,
        const object& continuation,
        const compilation_context in_a = as_is)
    {
      // if (in_a.program_declaration)
      // {
      //   std::cerr << "COMPILING DEFINITION IN A PROGRAM DECLARATION"
      //             << std::endl;
      // }

      if (not frames or in_a.program_declaration)
      {
        DEBUG_COMPILE(
          car(expression) << highlight::comment << "\t; is <variable>"
                          << attribute::normal << std::endl);
        return
          compile(
            cdr(expression) ? cadr(expression) : undefined,
            frames,
            cons(
              make<instruction>(mnemonic::DEFINE), car(expression),
              continuation));
      }
      else
      {
        throw syntax_error_about_internal_define {
          "definition cannot appear in this context"
        };
        // throw
        //   compile(
        //     cdr(expression) ? cadr(expression) : undefined,
        //     frames,
        //     cons(
        //       make<instruction>(mnemonic::DEFINE), car(expression),
        //       continuation));
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
        const object& frames,
        const object& continuation,
        const compilation_context in_a = as_is)
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
        if (in_a.program_declaration)
        {
          return
            compile(
              car(expression),
              frames,
              continuation,
              as_tail_expression_of_program_declaration);
        }
        else
        {
          return
            compile(
              car(expression),
              frames,
              continuation,
              as_tail_expression);
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
        if (in_a.program_declaration)
        {
          return
            sequence(
              cdr(expression),
              frames,
              continuation,
              as_program_declaration);
        }
        else
        {
          return
            sequence(
              cdr(expression),
              frames,
              continuation);
        }
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
        if (in_a.program_declaration)
        {
          return
            compile(
              car(expression),
              frames,
              cons(
                make<instruction>(mnemonic::POP),
                sequence(
                  cdr(expression),
                  frames,
                  continuation,
                  as_program_declaration)),
              as_program_declaration);
        }
        else
        {
          return
            compile(
              car(expression), // <non-definition expression>
              frames,
              cons(
                make<instruction>(mnemonic::POP), // remove result of expression
                sequence(
                  cdr(expression),
                  frames,
                  continuation)));
        }
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
            frames,
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
        const object& frames,
        const object& continuation,
        const compilation_context = as_is)
    {
      if (expression and expression.is<pair>())
      {
        return
          operand(
            cdr(expression),
            frames,
            compile(
              car(expression),
              frames,
              cons(
                make<instruction>(mnemonic::PUSH),
                continuation)));
      }
      else
      {
        return
          compile(
            expression,
            frames,
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
        const object& frames,
        const object& continuation,
        const compilation_context in_a = as_is)
    {
      DEBUG_COMPILE(
        car(expression) << highlight::comment << "\t; is <test>"
                        << attribute::normal << std::endl);

      if (in_a.tail_expression)
      {
        const auto consequent {
          compile(
            cadr(expression),
            frames,
            list(
              make<instruction>(mnemonic::RETURN)),
            as_tail_expression)
        };

        const auto alternate {
          cddr(expression)
            ? compile(
                caddr(expression),
                frames,
                list(
                  make<instruction>(mnemonic::RETURN)),
                as_tail_expression)
            : list(
                make<instruction>(mnemonic::LOAD_LITERAL), undefined,
                make<instruction>(mnemonic::RETURN))
        };

        return
          compile(
            car(expression), // <test>
            frames,
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
            frames,
            list(make<instruction>(mnemonic::JOIN)))
        };

        const auto alternate {
          cddr(expression)
            ? compile(
                caddr(expression),
                frames,
                list(
                  make<instruction>(mnemonic::JOIN)))
            : list(
                make<instruction>(mnemonic::LOAD_LITERAL), undefined,
                make<instruction>(mnemonic::JOIN))
        };

        return
          compile(
            car(expression), // <test>
            frames,
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
        const object& frames,
        const object& continuation,
        const compilation_context in_a = as_is)
    {
      DEBUG_COMPILE(
        car(expression) << highlight::comment << "\t; is <formals>"
                        << attribute::normal
                        << std::endl);
      return
        cons(
          make<instruction>(mnemonic::MAKE_CLOSURE),
          body(
            cdr(expression), // <body>
            cons(
              car(expression),
              frames), // extend lexical environment
            list( // continuation of body (finally, must be return)
              make<instruction>(mnemonic::RETURN)),
            in_a.program_declaration ? as_program_declaration : as_is),
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
        const object& frames,
        const object& continuation,
        const compilation_context = as_is)
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
            frames,
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
    // const object
    //   program(
    //     const object& expression,
    //     const object& frames,
    //     const object& continuation,
    //     const compilation_context = as_is)
    // {
    //   if (not cdr(expression)) // is tail sequence
    //   {
    //     try
    //     {
    //       return
    //         compile(
    //           car(expression),
    //           frames,
    //           continuation); // TODO tail-call-optimizable?
    //     }
    //     catch (const object& definition)
    //     {
    //       NEST_OUT;
    //       return definition;
    //     }
    //   }
    //   else
    //   {
    //     const auto declarations {
    //       program(
    //         cdr(expression),
    //         frames,
    //         continuation)
    //     };
    //     NEST_OUT;
    //
    //     try
    //     {
    //       return
    //         compile(
    //           car(expression),
    //           frames,
    //           cons(
    //             make<instruction>(mnemonic::POP), // remove result of expression
    //             declarations));
    //     }
    //     catch (const object& definition)
    //     {
    //       NEST_OUT;
    //       return
    //         cons(
    //           definition,
    //           make<instruction>(mnemonic::POP),
    //           declarations);
    //     }
    //   }
    // }

    /* ==== Call-With-Current-Syntactic-Continuation ==========================
    *
    * TODO documentation
    *
    *======================================================================== */
    const object
      call_csc(
        const object& expression,
        const object& frames,
        const object& continuation,
        const compilation_context = as_is)
    {
      DEBUG_COMPILE(
        car(expression) << highlight::comment << "\t; is <subprogram>"
                        << attribute::normal << std::endl);
      return
        compile(
          car(expression),
          frames,
          cons(
            make<instruction>(mnemonic::MAKE_SYNTACTIC_CONTINUATION),
            continuation),
          as_program_declaration);
    }

    /* ==== Fork ==============================================================
    *
    * TODO documentation
    *
    *======================================================================== */
    // const object
    //   fork(
    //     const object& expression,
    //     const object& frames,
    //     const object& continuation,
    //     const compilation_context = as_is)
    // {
    //   DEBUG_COMPILE(
    //     car(expression) << highlight::comment
    //                     << "\t; is <subprogram parameters>"
    //                     << attribute::normal
    //                     << std::endl);
    //   return
    //     cons(
    //       make<instruction>(mnemonic::FORK),
    //       program(
    //         cdr(expression),
    //         cons(car(expression), frames),
    //         list(make<instruction>(mnemonic::RETURN))),
    //       continuation);
    // }

    /* ==== Assignment ========================================================
    *
    * TODO documentation
    *
    *======================================================================== */
    const object
      assignment(
        const object& expression,
        const object& frames,
        const object& continuation,
        const compilation_context = as_is)
    {
      DEBUG_COMPILE(car(expression) << highlight::comment << "\t; is ");

      if (not expression)
      {
        throw syntax_error {"set!"};
      }
      else if (de_bruijn_index index {
                 car(expression), frames
               }; index)
      {
        if (index.is_variadic())
        {
          DEBUG_COMPILE_DECISION(
            "<identifier> of lexical variadic " << attribute::normal << index);

          return
            compile(
              cadr(expression),
              frames,
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
              frames,
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
            frames,
            cons(
              make<instruction>(mnemonic::SET_GLOBAL),
              car(expression),
              continuation));
      }
    }

    /* ==== Explicit Variable Reference =======================================
    *
    * TODO
    *
    *======================================================================== */
    const object
      reference(
        const object& expression,
        const object& frames,
        const object& continuation,
        const compilation_context = as_is)
    {
      DEBUG_COMPILE(car(expression) << highlight::comment << "\t; is ");

      if (not expression)
      {
        DEBUG_COMPILE_DECISION(
          "<identifier> of itself" << attribute::normal);

        return unit;
      }
      else if (de_bruijn_index<
                 equivalence_comparator<2>
               > variable {
                 car(expression),
                 frames
               }; variable)
      {
        if (variable.is_variadic())
        {
          DEBUG_COMPILE_DECISION(
            "<identifier> of local variadic " << attribute::normal << variable);

          return
            cons(
              make<instruction>(mnemonic::LOAD_LOCAL_VARIADIC), variable,
              continuation);
        }
        else
        {
          DEBUG_COMPILE_DECISION(
            "<identifier> of local " << attribute::normal << variable);

          return
            cons(
              make<instruction>(mnemonic::LOAD_LOCAL), variable,
              continuation);
        }
      }
      else
      {
        DEBUG_COMPILE_DECISION(
          "<identifier> of global variable" << attribute::normal);

        return
          cons(
            make<instruction>(mnemonic::LOAD_GLOBAL), car(expression),
            continuation);
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

