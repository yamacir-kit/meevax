#ifndef INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
#define INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/de_brujin_index.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/special.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/symbol.hpp> // object::is<symbol>()

inline namespace ugly_macros
{
  static std::size_t depth {0};

  #define DEBUG_COMPILE(...)                                                   \
  if (static_cast<SyntacticContinuation&>(*this).verbose.equivalent_to(true_object)) \
  {                                                                            \
    std::cerr << (not depth ? "; compile\t; " : ";\t\t; ")                     \
              << std::string(depth * 2, ' ')                                   \
              __VA_ARGS__;                                                     \
  }

  #define DEBUG_COMPILE_DECISION(...)                                          \
  if (static_cast<SyntacticContinuation&>(*this).verbose.equivalent_to(true_object))        \
  {                                                                            \
    std::cerr << __VA_ARGS__ << attribute::normal << std::endl;                \
  }

  #define DEBUG_MACROEXPAND(...)                                               \
  if (static_cast<SyntacticContinuation&>(*this).verbose.equivalent_to(true_object))        \
  {                                                                            \
    std::cerr << "; macroexpand\t; "                                           \
              << std::string(depth * 2, ' ')                                   \
              << __VA_ARGS__;                                                  \
  }

  #define COMPILER_WARNING(...) \
  if (static_cast<SyntacticContinuation&>(*this).verbose.equivalent_to(true_object))        \
  {                                                                            \
    std::cerr << attribute::normal  << "; "                                    \
              << highlight::warning << "compiler"                              \
              << attribute::normal  << "\t; "                                  \
              << highlight::warning << __VA_ARGS__                             \
              << attribute::normal  << std::endl;                              \
  }

  #define NEST_IN  ++depth
  #define NEST_OUT                                                             \
    DEBUG_COMPILE(                                                             \
      << highlight::syntax << ")" << attribute::normal << std::endl)           \
    --depth;
}

namespace meevax::kernel
{
  template <typename SyntacticContinuation>
  class machine // Simple SECD machine.
  {
  protected:
    object s, // Stack (holding intermediate results and return address)
           e, // Environment (giving values to symbols)
           c, // Code (instructions yet to be executed)
           d; // Dump (S.E.C)

  private: // CRTP
    #define CRTP(IDENTIFIER)                                                   \
    template <typename... Ts>                                                  \
    inline decltype(auto) IDENTIFIER(Ts&&... operands)                         \
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

      if (static_cast<const SyntacticContinuation&>(*this).verbose.equivalent_to(true_object))
      {
        std::cerr << "; define\t; "
                  << caar(interaction_environment())
                  << "\r\x1b[40C\x1b[K "
                  << cadar(interaction_environment())
                  << std::endl;
      }

      return interaction_environment(); // temporary
    }

    const object& innermost_dynamic_environment(const object& e)
    {
      for (const auto& frame : e)
      {
        if (frame and car(frame) and car(frame).is<SyntacticContinuation>())
        {
          return car(frame).as<SyntacticContinuation>().interaction_environment();
        }
      }

      return interaction_environment();
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

    const object& lookup_key(const object& identifier,
                             const object& environment)
    {
      if (not identifier or not environment)
      {
        return identifier;
      }
      else if (caar(environment) == identifier)
      {
        return caar(environment);
      }
      else
      {
        return lookup_key(identifier, cdr(environment));
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
      const object& syntactic_environment,
      const object& frames = unit,
      const object& continuation = list(make<instruction>(mnemonic::STOP)),
      const compilation_context in_a = as_is)
    {
      if (not expression)
      {
        return
          cons(
            make<instruction>(mnemonic::LOAD_CONSTANT), unit,
            continuation);
      }
      else if (not expression.is<pair>())
      {
        DEBUG_COMPILE(
          << expression << highlight::comment << "\t; ");

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
                  make<instruction>(mnemonic::LOAD_VARIADIC), index,
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
              make<instruction>(mnemonic::LOAD_CONSTANT), expression,
              continuation);
        }
      }
      else // is (application . arguments)
      {
        if (object applicant {lookup(
              car(expression), syntactic_environment
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
            << highlight::syntax << "(" << attribute::normal
            << car(expression)
            << highlight::comment << "\t; is <primitive expression> "
            << attribute::normal << applicant << std::endl);

          NEST_IN;
          auto result {std::invoke(applicant.as<special>(),
            cdr(expression), syntactic_environment, frames, continuation, in_a
          )};
          NEST_OUT;

          return result;
        }
        else if (applicant.is<SyntacticContinuation>()
                 and not de_bruijn_index(car(expression), frames))
        {
          DEBUG_COMPILE(
            << highlight::syntax << "(" << attribute::normal
            << car(expression)
            << highlight::comment << "\t; is <macro use> of <derived expression> "
            << attribute::normal << applicant
            << attribute::normal << std::endl);

          // std::cerr << "Syntactic-Continuation holds "
          //           << applicant.as<SyntacticContinuation>().continuation()
          //           << std::endl;

          const auto expanded {
            applicant.as<SyntacticContinuation>().expand(
              cons(
                applicant,
                cdr(expression)))
          };

          DEBUG_MACROEXPAND(expanded << std::endl);

          // NEST_IN;
          auto result {compile(
            expanded, syntactic_environment, frames, continuation)};
          // NEST_OUT;

          return result;
        }

        DEBUG_COMPILE(
          << highlight::syntax << "(" << attribute::normal
          << highlight::comment << "\t; is <procedure call>"
          << attribute::normal << std::endl);

        NEST_IN;
        auto result {
          operand(
            cdr(expression),
            syntactic_environment,
            frames,
            compile(
              car(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(
                  in_a.tail_expression ? mnemonic::TAIL_CALL
                                       : mnemonic::CALL),
                continuation)))
        };
        NEST_OUT;
        return result;
      }
    }

    void disassemble(const object& c, std::size_t depth = 1)
    {
      assert(0 < depth);

      for (homoiconic_iterator iter {c}; iter; ++iter)
      {
        std::cerr << "; ";

        if (iter == c)
        {
          std::cerr << std::string(4 * (depth - 1), ' ')
                    << highlight::syntax
                    << "("
                    << attribute::normal
                    << std::string(3, ' ');
        }
        else
        {
          std::cerr << std::string(4 * depth, ' ');
        }

        switch ((*iter).as<instruction>().code)
        {
        case mnemonic::CALL:
        case mnemonic::TAIL_CALL:
        case mnemonic::JOIN:
        case mnemonic::POP:
        case mnemonic::CONS:
          std::cerr << *iter << std::endl;
          break;

        case mnemonic::RETURN:
        case mnemonic::STOP:
          std::cerr << *iter
                    << highlight::syntax
                    << "\t)"
                    << attribute::normal
                    << std::endl;
          break;

        case mnemonic::DEFINE:
        case mnemonic::FORK:
        case mnemonic::LOAD_CONSTANT:
        case mnemonic::LOAD_GLOBAL:
        case mnemonic::LOAD_LOCAL:
        case mnemonic::LOAD_VARIADIC:
        case mnemonic::STORE_GLOBAL:
        case mnemonic::STORE_LOCAL:
        case mnemonic::STORE_VARIADIC:
          std::cerr << *iter << " " << *++iter << std::endl;
          break;

        case mnemonic::LOAD_CLOSURE:
        case mnemonic::LOAD_CONTINUATION:
          std::cerr << *iter << std::endl;
          disassemble(*++iter, depth + 1);
          break;


        case mnemonic::SELECT:
        case mnemonic::TAIL_SELECT:
          std::cerr << *iter << std::endl;
          disassemble(*++iter, depth + 1);
          disassemble(*++iter, depth + 1);
          break;

        default:
          assert(false);
        }
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

      if (static_cast<SyntacticContinuation&>(*this).verbose.equivalent_to(true_object))
      {
        // std::cerr << "; disassemble\t; for " << &c << std::endl;
        std::cerr << "; " << std::string(78, '*') << std::endl;
        disassemble(c);
        std::cerr << "; " << std::string(78, '*') << std::endl;
      }

      const auto result {execute()};

      s = pop(d);
      e = pop(d);
      c = pop(d);

      return result;
    }

    object execute()
    {
    dispatch:
      if (static_cast<SyntacticContinuation&>(*this)
            .trace.equivalent_to(true_object))
      {
        std::cerr << "; trace s\t; " <<  s << std::endl;
        std::cerr << ";       e\t; " <<  e << std::endl;
        std::cerr << ";       c\t; " <<  c << std::endl;
        std::cerr << ";       d\t; " <<  d << std::endl;
        std::cerr << std::endl;
      }

      switch (car(c).template as<instruction>().code)
      {
      /* ====*/ case mnemonic::LOAD_LOCAL: /*===================================
      *
      *              S  E (LOAD_LOCAL (i . j) . C) D
      *
      * => (result . S) E                       C  D
      *
      * where result = (list-ref (list-ref E i) j)
      *
      *====================================================================== */
        push(
          s,
          list_reference(
            list_reference(
              e,
              static_cast<int>(
                caadr(c).template as<real>())),
            static_cast<int>(
              cdadr(c).template as<real>())));
        pop<2>(c);
        goto dispatch;

      case mnemonic::LOAD_VARIADIC:
        push(
          s,
          list_tail(
            list_reference(
              e,
              static_cast<int>(
                caadr(c).template as<real>())),
            static_cast<int>(
              cdadr(c).template as<real>())));
        pop<2>(c);
        goto dispatch;

      case mnemonic::LOAD_CONSTANT: // S E (LOAD_CONSTANT constant . C) D => (constant . S) E C D
        push(s, cadr(c));
        pop<2>(c);
        goto dispatch;

      /* ====*/ case mnemonic::LOAD_GLOBAL: /*==================================
      *
      *              S  E (LOAD_GLOBAL identifier . C) D
      *
      * => (object . S) E                           C  D
      *
      *====================================================================== */
        if (const object value {
              std::invoke(
                cadr(c).template is<symbol>() ? assq : assoc,
                cadr(c),
                innermost_dynamic_environment(e))
            }; value != cadr(c))
        {
          push(s, cadr(value));
        }
        else // UNBOUND
        {
          push(s, rename(cadr(c)));
        }
        pop<2>(c);
        goto dispatch;

      /* ====*/ case mnemonic::LOAD_CLOSURE: /*=================================
      *
      *               S  E (LOAD_CLOSURE body . C) D
      *
      * => (closure . S) E                      C  D
      *
      *====================================================================== */
        push(
          s,
          make<closure>(
            cadr(c),
            e));
        pop<2>(c);
        goto dispatch;

      /* ====*/ case mnemonic::LOAD_CONTINUATION: /*============================
      *
      *                      S  E (LOADK cc . C) D
      *
      * => ((continuation) . S) E             C  D
      *
      *====================================================================== */
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

      /* ====*/ case mnemonic::FORK: /*=========================================
      *
      *                  S  E (FORK sc . C) D
      *
      * => (subprogram . S) E            C  D
      *
      *====================================================================== */
        // std::cerr << "; FORK\t; making syntactic-continuation" << std::endl;
        // std::cerr << "\t\t; s = " << s << std::endl;
        // std::cerr << "\t\t; e = " << e << std::endl;
        // std::cerr << "\t\t; c = " << c << std::endl;
        // std::cerr << "\t\t; d = " << d << std::endl;
        push(
          s,
          make<SyntacticContinuation>(
            cons(
              s,
              e,
              cadr(c), // compile continuation
              d),
            interaction_environment()));
        pop<2>(c);
        goto dispatch;

      // /* ====*/ case mnemonic::FORK: /*=========================================
      // *
      // *    (closure . S) E (FORK . C) D
      // *
      // * => (program . S) E         C  D
      // *
      // *====================================================================== */
      //   push(
      //     s,
      //     make<SyntacticContinuation>(
      //       pop(s), // XXX car(s)?
      //       interaction_environment()));
      //   pop<1>(c);
      //   goto dispatch;

      /* ====*/ case mnemonic::SELECT: /*=======================================
      *
      *    (test . S) E (SELECT consequent alternate . C)  D
      *
      * =>         S  E         selection             (C . D)
      *
      * where selection = (if test consequent alternate)
      *
      *====================================================================== */
        push(d, cdddr(c));
        [[fallthrough]];

      /* ====*/ case mnemonic::TAIL_SELECT: /*==================================
      *
      *    (test . S) E (SELECT consequent alternate . C)  D
      *
      * =>         S  E         selection                  D
      *
      * where selection = (if test consequent alternate)
      *
      *====================================================================== */
        c = not car(s).equivalent_to(false_object) ? cadr(c) : caddr(c);
        pop<1>(s);
        goto dispatch;

      /* ====*/ case mnemonic::JOIN: /*=========================================
      *
      *    S E (JOIN . C) (C' . D)
      *
      * => S E             C'   D
      *
      *====================================================================== */
        c = car(d);
        pop<1>(d);
        goto dispatch;

      /* ====*/ case mnemonic::DEFINE: /*=======================================
      *
      *        (object . S) E (DEFINE identifier . C) D
      *
      * => (identifier . S) E                      C  D
      *
      *====================================================================== */
        if (static_cast<SyntacticContinuation&>(*this).virgin)
        {
          define(cadr(c), car(s));
          car(s) = unspecified;
        }
        else
        {
          std::cerr << "; define\t; redefinition of " << cadr(c) << " is ignored" << std::endl;
        }
        pop<2>(c);
        goto dispatch;

      case mnemonic::CALL:
        if (const object callee {car(s)}; not callee)
        {
          static const error e {"unit is not appliciable"};
          throw e;
        }
        else if (callee.is<closure>()) // (closure operands . S) E (CALL . C) D
        {
          push(d, cddr(s), e, cdr(c));
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure operands . S) E (CALL . C) D => (result . S) E C D
        {
          s = cons(
                std::invoke(
                  callee.as<procedure>(),
                  resource {},
                  cadr(s)),
                cddr(s));
          pop<1>(c);
        }
        else if (callee.is<SyntacticContinuation>()) // TODO REMOVE
        {
          // s = cons(
          //       callee.as<SyntacticContinuation>().expand(
          //         cons(
          //           car(s),
          //           cadr(s))),
          //       cddr(s));
          s = cons(
                callee.as<SyntacticContinuation>().evaluate( // TODO expand => evaluate ?
                  cadr(s)),
                cddr(s));
          pop<1>(c);
        }
        else if (callee.is<continuation>()) // (continuation operands . S) E (CALL . C) D
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

      case mnemonic::TAIL_CALL:
        if (object callee {car(s)}; not callee)
        {
          throw evaluation_error {"unit is not appliciable"};
        }
        else if (callee.is<closure>()) // (closure operands . S) E (CALL . C) D
        {
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure operands . S) E (CALL . C) D => (result . S) E C D
        {
          s = cons(
                std::invoke(
                  callee.as<procedure>(),
                  resource {},
                  cadr(s)),
                cddr(s));
          pop<1>(c);
        }
        else if (callee.is<SyntacticContinuation>()) // TODO REMOVE
        {
          // s = cons(
          //       callee.as<SyntacticContinuation>().expand(
          //         cons(
          //           car(s),
          //           cadr(s))),
          //       cddr(s));
          s = cons(
                callee.as<SyntacticContinuation>().evaluate(
                  cadr(s)),
                cddr(s));
          pop<1>(c);
        }
        else if (callee.is<continuation>()) // (continuation operands . S) E (CALL . C) D
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

      /* ====*/ case mnemonic::RETURN: /*=======================================
      *
      *    (result . S)  E (RETURN . C) (S' E' C' . D)
      *
      * => (result . S') E'          C'             D
      *
      *====================================================================== */
        s = cons(
              car(s), // The result of procedure
              pop(d));
        e = pop(d);
        c = pop(d);
        goto dispatch;

      /* ====*/ case mnemonic::CONS: /*=========================================
      *
      *    ( X   Y  . S) E (CONS . C) D
      *
      * => ((X . Y) . S) E         C  D
      *
      *====================================================================== */
        s = cons(cons(car(s), cadr(s)), cddr(s));
        pop<1>(c);
        goto dispatch;

      /* ====*/ case mnemonic::POP: /*==========================================
      *
      *    (result . S) E (POP . C) D
      *
      * =>           S  E        C  D
      *
      *====================================================================== */
        pop<1>(s);
        pop<1>(c);
        goto dispatch;

      /* ====*/ case mnemonic::STORE_GLOBAL: /*=================================
      *
      *          (value . S) E (STORE-GLOBAL identifier . C) D
      *
      * => (unspecified . S) E                            C  D
      *
      * TODO
      * (1) There is no need to make copy if right hand side is unique.
      * (2) There is no matter overwrite if left hand side is unique.
      * (3) Should set with weak reference if right hand side is newer.
      *
      *====================================================================== */
        if (const object value {
              assq(cadr(c), innermost_dynamic_environment(e))
            }; value != cdadr(c))
        {
          std::atomic_store(&cadr(value), car(s).copy());
        }
        else // UNBOUND
        {
          define(cadr(c), car(s));
        }
        // car(s) = unspecified;
        pop<2>(c);
        goto dispatch;

      case mnemonic::STORE_LOCAL: // (value . S) E (STORE_LOCAL (i . j) . C) D => (value . S) E C D
        std::atomic_store(
          &car(
            list_tail(
              list_reference(
                e,
                static_cast<int>(
                  caadr(c).template as<real>())),
              static_cast<int>(
                cdadr(c).template as<real>()))),
          car(s));
        pop<2>(c);
        goto dispatch;

      case mnemonic::STORE_VARIADIC:
        std::atomic_store(
          &cdr(
            list_tail(
              list_reference(
                e,
                static_cast<int>(
                  caadr(c).template as<real>())),
              static_cast<int>(
                cdadr(c).template as<real>() - 1))),
          car(s));
        pop<2>(c);
        goto dispatch;

      /* ====*/ case mnemonic::STOP: /*=========================================
      *
      * (result . S) E (STOP . C) D
      *
      *====================================================================== */
      default:
        pop<1>(c);
        return pop(s);
        // return car(s);
      }
    }

  protected: // Primitive Expression Types
    #define DEFINE_PRIMITIVE_EXPRESSION(NAME, ...)                             \
    const object NAME(                                                         \
      [[maybe_unused]] const object& expression,                               \
      [[maybe_unused]] const object& syntactic_environment,                    \
      [[maybe_unused]] const object& frames,                                   \
      [[maybe_unused]] const object& continuation,                             \
      [[maybe_unused]] const compilation_context in_a = as_is)                 \
    __VA_ARGS__

    /* ==== Quotation =========================================================
    *
    * <quotation> = (quote <datum>)
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(quotation,
    {
      DEBUG_COMPILE(
        << car(expression)
        << highlight::comment << "\t; is <datum>"
        << attribute::normal << std::endl);

      return
        cons(
          make<instruction>(mnemonic::LOAD_CONSTANT), car(expression),
          continuation);
    })

    /* ==== Sequence ==========================================================
    *
    * <sequence> = <command>* <expression>
    *
    * <command> = <expression>
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(sequence,
    {
      if (in_a.program_declaration)
      {
        if (not cdr(expression))
        {
          return
            compile(
              car(expression),
              syntactic_environment,
              frames,
              continuation,
              as_program_declaration);
        }
        else
        {
          return
            compile(
              car(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::POP),
                sequence(
                  cdr(expression),
                  syntactic_environment,
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
              syntactic_environment,
              frames,
              continuation);
        }
        else
        {
          return
            compile(
              car(expression), // head expression
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::POP), // pop result of head expression
                sequence(
                  cdr(expression), // rest expressions
                  syntactic_environment,
                  frames,
                  continuation)));
        }
      }
    })

    /* ==== Definition ========================================================
    *
    * <definition> = (define <identifier> <expression>)
    *
    * TODO MOVE INTO SYNTACTIC_CONTINUATION
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(definition,
    {
      if (not frames or in_a.program_declaration)
      {
        DEBUG_COMPILE(
          << car(expression)
          << highlight::comment << "\t; is <variable>"
          << attribute::normal << std::endl);

        // const auto definition {compile(
        //   cdr(expression) ? cadr(expression) : unspecified,
        //   syntactic_environment,
        //   frames,
        //   list(
        //     make<instruction>(mnemonic::DEFINE), car(expression),
        //     make<instruction>(mnemonic::STOP))
        // )};
        //
        // object result {unit};
        //
        // if (in_a.program_declaration)
        // {
        //   c = definition;
        //
        //   // std::cerr << ";\t\t; s = " << s << std::endl;
        //   // std::cerr << ";\t\t; e = " << e << std::endl;
        //   // std::cerr << ";\t\t; c = " << c << std::endl;
        //   // std::cerr << ";\t\t; d = " << d << std::endl;
        //
        //   result = execute();
        // }
        // else
        // {
        //   result = execute_interrupt(definition);
        // }
        //
        // return
        //   cons(
        //     make<instruction>(mnemonic::LOAD_CONSTANT), result,
        //     continuation);

        return
          compile(
            cdr(expression) ? cadr(expression) : unspecified,
            syntactic_environment,
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
        //     syntactic_environment,
        //     frames,
        //     cons(
        //       make<instruction>(mnemonic::DEFINE), car(expression),
        //       continuation));
      }
    })

    /* ==== Lambda Body =======================================================
    *
    * <body> = <definition>* <sequence>
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(body,
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
              syntactic_environment,
              frames,
              continuation,
              as_tail_expression_of_program_declaration);
        }
        else
        {
          return
            compile(
              car(expression),
              syntactic_environment,
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
              syntactic_environment,
              frames,
              continuation,
              as_program_declaration);
        }
        else
        {
          return
            sequence(
              cdr(expression),
              syntactic_environment,
              frames,
              continuation);
        }
      }
      // XXX DIRTY HACK
      else if (car(expression).is<pair>() // is application
               and caar(expression) // the operator is not illegal
               and caar(expression).template is<symbol>() // the operator is variable reference
               and not de_bruijn_index( // the operator is not local variable
                         caar(expression),
                         frames)
               and lookup( // the variable references special form
                     caar(expression),
                     syntactic_environment)
                   .template is<special>()
               and lookup( // the special form is "define"
                     caar(expression),
                     syntactic_environment)
                   .template as<special>()
                   .name == "define")
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
            syntactic_environment,
            frames,
            continuation);
      }
      else
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
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::POP),
                sequence(
                  cdr(expression),
                  syntactic_environment,
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
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::POP), // remove result of expression
                sequence(
                  cdr(expression),
                  syntactic_environment,
                  frames,
                  continuation)));
        }
      }
    })

    /* ==== Operand ===========================================================
    *
    * <operand> = <expression>
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(operand,
    {
      if (expression and expression.is<pair>())
      {
        return
          operand(
            cdr(expression),
            syntactic_environment,
            frames,
            compile(
              car(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::CONS),
                continuation)));
      }
      else
      {
        return
          compile(
            expression,
            syntactic_environment,
            frames,
            continuation);
      }
    })

    /* ==== Conditional =======================================================
    *
    * <conditional> = (if <test> <consequent> <alternate>)
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(conditional,
    {
      DEBUG_COMPILE(
        << car(expression)
        << highlight::comment << "\t; is <test>"
        << attribute::normal << std::endl);

      if (in_a.tail_expression)
      {
        const auto consequent {
          compile(
            cadr(expression),
            syntactic_environment,
            frames,
            list(
              make<instruction>(mnemonic::RETURN)),
            as_tail_expression)
        };

        const auto alternate {
          cddr(expression)
            ? compile(
                caddr(expression),
                syntactic_environment,
                frames,
                list(
                  make<instruction>(mnemonic::RETURN)),
                as_tail_expression)
            : list(
                make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                make<instruction>(mnemonic::RETURN))
        };

        return
          compile(
            car(expression), // <test>
            syntactic_environment,
            frames,
            cons(
              make<instruction>(mnemonic::TAIL_SELECT),
              consequent,
              alternate,
              cdr(continuation)));
      }
      else
      {
        const auto consequent {
          compile(
            cadr(expression),
            syntactic_environment,
            frames,
            list(make<instruction>(mnemonic::JOIN)))
        };

        const auto alternate {
          cddr(expression)
            ? compile(
                caddr(expression),
                syntactic_environment,
                frames,
                list(
                  make<instruction>(mnemonic::JOIN)))
            : list(
                make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                make<instruction>(mnemonic::JOIN))
        };

        return
          compile(
            car(expression), // <test>
            syntactic_environment,
            frames,
            cons(
              make<instruction>(mnemonic::SELECT), consequent, alternate,
              continuation));
      }
    })

    /* ==== Lambda Expression =================================================
    *
    * <lambda expression> = (lambda <formals> <body>)
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(lambda,
    {
      DEBUG_COMPILE(
        << car(expression)
        << highlight::comment << "\t; is <formals>"
        << attribute::normal << std::endl);

      if (in_a.program_declaration)
      {
        return
          cons(
            make<instruction>(mnemonic::LOAD_CLOSURE),
            body(
              cdr(expression),
              syntactic_environment,
              cons(
                car(expression), // <formals>
                frames),
              list(
                make<instruction>(mnemonic::RETURN)),
              as_program_declaration),
            continuation);
      }
      else
      {
        return
          cons(
            make<instruction>(mnemonic::LOAD_CLOSURE),
            body(
              cdr(expression),
              syntactic_environment,
              cons(
                car(expression), // <formals>
                frames),
              list(
                make<instruction>(mnemonic::RETURN))),
            continuation);
      }
    })

    /* ==== Call-With-Current-Continuation ====================================
    *
    * TODO documentation
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(call_cc,
    {
      DEBUG_COMPILE(
        << car(expression)
        << highlight::comment << "\t; is <procedure>"
        << attribute::normal << std::endl);

      return
        cons(
          make<instruction>(mnemonic::LOAD_CONTINUATION),
          continuation,
          compile(
            car(expression),
            syntactic_environment,
            frames,
            cons(
              make<instruction>(mnemonic::CALL),
              continuation)));
    })

    /* ==== Fork ===============================================================
    *
    * TODO documentation
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(fork,
    {
      DEBUG_COMPILE(
        << car(expression)
        << highlight::comment << "\t; is <subprogram>"
        << attribute::normal << std::endl);

      return
        cons(
          make<instruction>(mnemonic::FORK), cons(car(expression), frames),
          continuation);

      // return
      //   compile(
      //     car(expression),
      //     syntactic_environment,
      //     frames,
      //     cons(
      //       make<instruction>(mnemonic::FORK),
      //       continuation),
      //     as_program_declaration);
    })

    /* ==== Assignment ========================================================
    *
    * TODO documentation
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(assignment,
    {
      DEBUG_COMPILE(<< car(expression) << highlight::comment << "\t; is ");

      if (not expression)
      {
        throw syntax_error {"set!"};
      }
      else if (de_bruijn_index index {car(expression), frames}; index)
      {
        if (index.is_variadic())
        {
          DEBUG_COMPILE_DECISION(
            "<identifier> of lexical variadic " << attribute::normal << index);

          return
            compile(
              cadr(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::STORE_VARIADIC), index,
                continuation));
        }
        else
        {
          DEBUG_COMPILE_DECISION("<identifier> of lexical " << attribute::normal << index);

          return
            compile(
              cadr(expression),
              syntactic_environment,
              frames,
              cons(
                make<instruction>(mnemonic::STORE_LOCAL), index,
                continuation));
        }
      }
      else
      {
        DEBUG_COMPILE_DECISION("<identifier> of dynamic variable " << attribute::normal);

        return
          compile(
            cadr(expression),
            syntactic_environment,
            frames,
            cons(
              make<instruction>(mnemonic::STORE_GLOBAL), car(expression),
              continuation));
      }
    })

    /* ==== Explicit Variable Reference =======================================
    *
    * TODO DEPRECATED
    * TODO REMOVE AFTER IMPLEMENTED MODULE SYSTEM
    *
    *======================================================================== */
    DEFINE_PRIMITIVE_EXPRESSION(reference,
    {
      DEBUG_COMPILE(<< car(expression) << highlight::comment << "\t; is ");

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
              make<instruction>(mnemonic::LOAD_VARIADIC), variable,
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
    })
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

