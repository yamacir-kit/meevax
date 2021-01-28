#ifndef INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
#define INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/de_brujin_index.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/identifier.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/syntax.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename SK>
  class machine // TR-SECD machine.
  {
    friend SK;

    machine()
    {}

    IMPORT(SK, debug,);
    IMPORT(SK, evaluate,);
    IMPORT(SK, header, const);
    IMPORT(SK, in_trace_mode, const);
    IMPORT(SK, indent,);
    IMPORT(SK, intern,);
    IMPORT(SK, rename,);
    IMPORT(SK, shift, const);
    IMPORT(SK, standard_debug_port, const);
    IMPORT(SK, standard_error_port, const);
    IMPORT(SK, standard_output_port, const);
    IMPORT(SK, syntactic_environment,);
    IMPORT(SK, write_to, const);

    using keyword = SK;

  protected:
    let s, // Stack (holding intermediate results and return address)
        e, // Environment (giving values to symbols)
        c, // Control (instructions yet to be executed)
        d; // Dump (S.E.C)

  public:
    template <typename... Ts>
    let const& define(object const& variable, Ts&&... expression)
    {
      push(
        syntactic_environment(),
        cons(variable, std::forward<decltype(expression)>(expression)...));

      write_to(standard_debug_port(),
        header("define"),
        caar(syntactic_environment()),
        faint, " binds ", reset,
        cdar(syntactic_environment()),
        "\n");

      return unspecified;
    }

    /* ---- Auxiliary Syntax 'global' ------------------------------------------
     *
     *  Note: This function extends the given syntax environment. Since the
     *  order of operand evaluation in C ++ is undefined, be aware of the
     *  execution timing of side effects of this function.
     *
     * ---------------------------------------------------------------------- */
    let const global(let const& x, let & syntactic_environment)
    {
      if (let const binding = assq(x, syntactic_environment); not eq(binding, f))
      {
        return binding;
      }
      else // unbound
      {
        return global(x, push(syntactic_environment, cons(x, make<syntactic_closure>(x, syntactic_environment))));
      }
    }

    let const& glocal_environment(object const& e)
    {
      for (auto const& frame : e)
      {
        if (frame.is<pair>() and car(frame).is<SK>())
        {
          return cdar(frame); // SAME-AS car(frame).as<SK>().syntactic_environment();
        }
      }

      return syntactic_environment();
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
    let compile(
      syntactic_context const& the_expression_is,
      let const& expression,
      let      & syntactic_environment,
      let const& frames = unit,
      let const& continuation = list(make<instruction>(mnemonic::STOP)))
    {
      if (expression.is<null>())
      {
        return cons(make<instruction>(mnemonic::LOAD_CONSTANT), unit, continuation);
      }
      else if (not expression.is<pair>()) // is <identifier>
      {
        if (is_identifier(expression))
        {
          if (de_bruijn_index index { expression, frames }; index)
          {
            if (index.is_variadic())
            {
              debug(expression, faint, " ; is a <variadic bound variable> references ", reset, index);
              return cons(make<instruction>(mnemonic::LOAD_VARIADIC), index, continuation);
            }
            else
            {
              debug(expression, faint, " ; is a <bound variable> references ", reset, index);
              return cons(make<instruction>(mnemonic::LOAD_LOCAL), index, continuation);
            }
          }
          else if (expression.is<syntactic_closure>())
          {
            debug(expression, faint, " ; is <alias>");
            return cons(make<instruction>(mnemonic::STRIP), expression, continuation);
          }
          else
          {
            debug(expression, faint, " ; is a <glocal variable>");
            return cons(make<instruction>(mnemonic::LOAD_GLOBAL), global(expression, syntactic_environment), continuation);
          }
        }
        else // is <self-evaluating>
        {
          debug(expression, faint, " ; is <self-evaluating>");
          return cons(make<instruction>(mnemonic::LOAD_CONSTANT), expression, continuation);
        }
      }
      else // is (applicant . arguments)
      {
        if (let const& applicant = lookup(car(expression), syntactic_environment);
            not applicant.is<null>() and not de_bruijn_index(car(expression), frames))
        {
          if (applicant.is<syntax>())
          {
            debug(magenta, "(", reset, car(expression), faint, " ; is <primitive expression>");

            indent() >> shift();

            decltype(auto) result =
              applicant.as<syntax>().compile(the_expression_is,
                cdr(expression), syntactic_environment, frames, continuation);

            debug(magenta, ")");

            indent() << shift();

            return result;
          }
          else if (applicant.is<SK>())
          {
            debug(magenta, "(", reset, car(expression), faint, " ; is <macro application>");

            const auto expanded {
              // applicant.as<SK>().expand(cons(applicant, cdr(expression)))
              applicant.as<SK>().expand(applicant, expression)
            };

            // debug(expanded);

            write_to(standard_debug_port(),
              header("macroexpand-1"), indent(), expanded, "\n");

            return compile(in_context_free, expanded, syntactic_environment, frames, continuation);
          }
        }

        debug(magenta, "(", reset, faint, " ; is <procedure call>");
        indent() >> shift();

        decltype(auto) result =
          operand(in_context_free,
                  cdr(expression),
                  syntactic_environment,
                  frames,
                  compile(in_context_free,
                          car(expression),
                          syntactic_environment,
                          frames,
                          cons(make<instruction>(the_expression_is.in_a_tail_context() ? mnemonic::TAIL_CALL
                                                                                       : mnemonic::     CALL),
                               continuation)));

        debug(magenta, ")");
        indent() << shift();

        return result;
      }
    }

    template
    <
      bool Trace = false
    >
    let execute()
    {
    dispatch:
      if constexpr (Trace)
      {
        std::cerr << "; trace s\t; " <<  s << "\n"
                  << ";       e\t; " <<  e << "\n"
                  << ";       c\t; " <<  c << "\n"
                  << ";       d\t; " <<  d << "\n" << std::endl;
      }

      switch (car(c).template as<instruction>().code)
      {
      case mnemonic::LOAD_LOCAL: /* --------------------------------------------
        *
        *               S  E (LOAD-LOCAL (i . j) . C) D
        *  => (result . S) E                       C  D
        *
        *  where result = (list-ref (list-ref E i) j)
        *
        *  i = (caadr c)
        *  j = (cdadr c)
        *
        * ------------------------------------------------------------------- */
        push(s, list_ref(list_ref(e, caadr(c)), cdadr(c)));
        c = cddr(c);
        goto dispatch;

      case mnemonic::LOAD_VARIADIC: /* -----------------------------------------
        *
        *               S  E (LOAD-VARIADIC (i . j) . C) D
        *  => (result . S) E                          C  D
        *
        *  where result = (list-tail (list-ref E i) j)
        *
        * ------------------------------------------------------------------- */
        push(s, list_tail(list_ref(e, caadr(c)), cdadr(c)));
        c = cddr(c);
        goto dispatch;

      case mnemonic::LOAD_CONSTANT: /* -----------------------------------------
        *
        *                 S  E (LOAD-CONSTANT constant . C) D
        *  => (constant . S) E                           C  D
        *
        * ------------------------------------------------------------------- */
        // push(s, strip(cadr(c)));
        push(s, cadr(c));
        c = cddr(c);
        goto dispatch;

      // case mnemonic::LOAD_SYNTAX: /* -------------------------------------------
      //   *
      //   *                 S  E (LOAD-CONSTANT syntax . C) D
      //   *  => (constant . S) E                         C  D
      //   *
      //   * ------------------------------------------------------------------- */
      //   push(s, cadr(c));
      //   pop<2>(c);
      //   goto dispatch;

      case mnemonic::LOAD_GLOBAL: /* -------------------------------------------
        *
        *               S  E (LOAD-GLOBAL cell . C) D
        *  => (object . S) E                     C  D
        *
        * ------------------------------------------------------------------- */
        push(s, cdadr(c));
        c = cddr(c);
        goto dispatch;

      case mnemonic::STRIP: /* -------------------------------------------------
        *
        *             S  E (STRIP syntactic-closure . C) D
        *  => (form . S) E                            C  D
        *
        * ------------------------------------------------------------------- */
        push(s, cadr(c).template as<syntactic_closure>().strip());
        c = cddr(c);
        goto dispatch;

      case mnemonic::LOAD_CLOSURE: /* ------------------------------------------
        *
        *                S  E (LOAD-CLOSURE body . C) D
        *  => (closure . S) E                      C  D
        *
        * ------------------------------------------------------------------- */
        push(s, make<closure>(cadr(c), e));
        c = cddr(c);
        goto dispatch;

      case mnemonic::LOAD_CONTINUATION: /* -------------------------------------
        *
        *                       S  E (LDK cc . C) D
        *  => ((continuation) . S) E           C  D
        *
        * ------------------------------------------------------------------- */
        push(s, list(make<continuation>(s, cons(e, cadr(c), d))));
        c = cddr(c);
        goto dispatch;

      case mnemonic::FORK: /* --------------------------------------------------
        *
        *                   S  E (FORK csc . C) D
        *  => (subprogram . S) E             C  D
        *
        * ------------------------------------------------------------------- */
        push(s, make<SK>(cons(s, e, cadr(c), d), syntactic_environment()));
        c = cddr(c);
        goto dispatch;

      case mnemonic::SELECT: /* ------------------------------------------------
        *
        *     (test . S) E (SELECT consequent alternate . C)  D
        *  =>         S  E         selection             (C . D)
        *
        *  where selection = (if test consequent alternate)
        *
        * ------------------------------------------------------------------- */
        push(d, cdddr(c));
        [[fallthrough]];

      case mnemonic::TAIL_SELECT: /* -------------------------------------------
        *
        *     (test . S) E (SELECT consequent alternate . C)  D
        *  =>         S  E         selection                  D
        *
        *  where selection = (if test consequent alternate)
        *
        * ------------------------------------------------------------------- */
        c = car(s).template is<null>() or not car(s).eqv(f) ? cadr(c) : caddr(c);
        s = cdr(s);
        goto dispatch;

      case mnemonic::JOIN: /* --------------------------------------------------
        *
        *     S E (JOIN) (C . D)
        *  => S E         C   D
        *
        * ------------------------------------------------------------------- */
        c = car(d);
        d = cdr(d);
        goto dispatch;

      case mnemonic::DEFINE: /* ------------------------------------------------
        *
        *         (object . S) E (DEFINE cell . C) D
        *  => (identifier . S) E                C  D
        *
        *  where cell = (identifier . identifier)
        *
        * ------------------------------------------------------------------- */
        if (static_cast<SK&>(*this).generation == 0)
        {
          cdadr(c) = car(s);
          // car(s) = caadr(c);
        }
        c = cddr(c);
        goto dispatch;

      case mnemonic::CALL: /* --------------------------------------------------
        *
        *
        * ------------------------------------------------------------------- */
        if (let const& callee = car(s); callee.is<closure>()) // (closure operands . S) E (CALL . C) D
        {
          push(d, cddr(s), e, cdr(c));
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure operands . S) E (CALL . C) D => (result . S) E C D
        {
          s = cons(std::invoke(callee.as<procedure>(), cadr(s)), cddr(s));
          c = cdr(c);
        }
        else if (callee.is<continuation>()) // (continuation operands . S) E (CALL . C) D
        {
          s = cons(
                caadr(s),
                car(callee));
          e =  cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
        }
        else
        {
          throw error(callee, " is not applicable");
        }
        goto dispatch;

      case mnemonic::TAIL_CALL: /* ---------------------------------------------
        *
        *
        * ------------------------------------------------------------------- */
        if (let const& callee = car(s); callee.is<closure>()) // (closure operands . S) E (CALL . C) D
        {
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) // (procedure operands . S) E (CALL . C) D => (result . S) E C D
        {
          s = cons(std::invoke(callee.as<procedure>(), cadr(s)), cddr(s));
          c = cdr(c);
        }
        else if (callee.is<continuation>()) // (continuation operands . S) E (CALL . C) D
        {
          s = cons(caadr(s),
                car(callee));
          e =  cadr(callee);
          c = caddr(callee);
          d = cdddr(callee);
        }
        else
        {
          throw error(callee, " is not applicable");
        }
        goto dispatch;

      case mnemonic::RETURN: /* ------------------------------------------------
        *
        *     (result . S)  E (RETURN . C) (S' E' C' . D)
        *  => (result . S') E'          C'             D
        *
        * ------------------------------------------------------------------- */
        s = cons(car(s), pop(d));
        e = pop(d);
        c = pop(d);
        goto dispatch;

      case mnemonic::CONS: /* --------------------------------------------------
        *
        *     ( X   Y  . S) E (CONS . C) D
        *  => ((X . Y) . S) E         C  D
        *
        * ------------------------------------------------------------------- */
        s = cons(cons(car(s), cadr(s)), cddr(s));
        c = cdr(c);
        goto dispatch;

      case mnemonic::DROP: /* --------------------------------------------------
        *
        *     (result . S) E (DROP . C) D
        *   =>          S  E         C  D
        *
        * ------------------------------------------------------------------- */
        s = cdr(s);
        c = cdr(c);
        goto dispatch;

      case mnemonic::STORE_GLOBAL: /* ------------------------------------------
        *
        *     (value . S) E (STORE-GLOBAL cell . C) D
        *  => (value . S) E                      C  D
        *
        *  where cell = (identifier . x)
        *
        * ------------------------------------------------------------------- */
        if (let const& binding = cadr(c); cdr(binding).is<null>() or car(s).template is<null>())
        {
          cdr(binding) = car(s);
        }
        else if (cdr(binding).is<keyword>() or cdr(binding).is<syntax>())
        {
          /* ---- From R7RS 5.3.1. Top level definitions ---------------------
           *
           *  However, if <variable> is not bound, or is a syntactic keyword,
           *  then the definition will bind <variable> to a new location
           *  before performing the assignment, whereas it would be an error
           *  to perform a set! on an unbound variable.
           *
           * -------------------------------------------------------------- */
          define(cadr(c), car(s));
        }
        else
        {
          std::atomic_store(&cdr(binding), car(s).copy());
        }
        c = cddr(c);
        goto dispatch;

      case mnemonic::STORE_LOCAL: /* -------------------------------------------
        *
        *      (value . S) E (STORE-LOCAL (i . j) . C) D
        *   => (value . S) E                        C  D
        *
        * ------------------------------------------------------------------- */
        std::atomic_store(&car(list_tail(list_ref(e, caadr(c)), cdadr(c))), car(s));
        c = cddr(c);
        goto dispatch;

      case mnemonic::STORE_VARIADIC:
        std::atomic_store(&cdr(list_tail(list_ref(e, caadr(c)), cdadr(c))), car(s));
        c = cddr(c);
        goto dispatch;

      default: // ERROR
      case mnemonic::STOP: /* --------------------------------------------------
        *
        *     (result . S) E (STOP . C) D
        * = >           S  E         C  D
        *
        * ------------------------------------------------------------------- */
        c = cdr(c);
        return pop(s); // return car(s);
      }
    }

  protected: // Primitive Expression Types
    /* ---- Quotation ----------------------------------------------------------
     *
     *  <quotation> = (quote <datum>)
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(quotation)
    {
      debug(car(expression), faint, " ; is <datum>");
      return cons(make<instruction>(mnemonic::LOAD_CONSTANT), car(expression), continuation);
    }

    /* ---- Sequence -----------------------------------------------------------
     *
     *  <sequence> = <command>* <expression>
     *
     *  <command> = <expression>
     *
     *  Note: The return value of <Command> is discarded.
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(sequence)
    {
      if (the_expression_is.at_the_top_level())
      {
        if (cdr(expression).is<null>())
        {
          return compile(at_the_top_level,
                         car(expression),
                         syntactic_environment,
                         frames,
                         continuation);
        }
        else
        {
          return compile(at_the_top_level,
                         car(expression),
                         syntactic_environment,
                         frames,
                         cons(make<instruction>(mnemonic::DROP),
                              sequence(at_the_top_level,
                                       cdr(expression),
                                       syntactic_environment,
                                       frames,
                                       continuation)));
        }
      }
      else
      {
        if (cdr(expression).is<null>()) // is tail sequence
        {
          return compile(the_expression_is,
                         car(expression),
                         syntactic_environment,
                         frames,
                         continuation);
        }
        else
        {
          return compile(in_context_free,
                         car(expression), // head expression
                         syntactic_environment,
                         frames,
                         cons(make<instruction>(mnemonic::DROP), // pop result of head expression
                              sequence(in_context_free,
                                       cdr(expression), // rest expressions
                                       syntactic_environment,
                                       frames,
                                       continuation)));
        }
      }
    }

    enum class internal_definition_tag {};

    SYNTAX(definition) /* ------------------------------------------------------
    *
    *  <definition> = (define <identifier> <expression>)
    *
    * ----------------------------------------------------------------------- */
    {
      if (frames.is<null>() or the_expression_is.at_the_top_level())
      {
        debug(car(expression), faint, " ; is <variable>");

        if (car(expression).is<pair>()) // (define (f . <formals>) <body>)
        {
          let const g = global(caar(expression), syntactic_environment);

          return compile(in_context_free,
                         cons(intern("lambda"), cdar(expression), cdr(expression)),
                         syntactic_environment,
                         frames,
                         cons(make<instruction>(mnemonic::DEFINE), g,
                              continuation));
        }
        else // (define x ...)
        {
          let const g = global(car(expression), syntactic_environment);

          return compile(in_context_free,
                         cdr(expression) ? cadr(expression) : unspecified,
                         syntactic_environment,
                         frames,
                         cons(make<instruction>(mnemonic::DEFINE), g,
                              continuation));
        }
      }
      else
      {
        indent() << shift(); // XXX DIRTY HACK!
        throw syntax_error<internal_definition_tag>("definition cannot appear in this context");
      }
    }

    /* ---- Lambda Body --------------------------------------------------------
     *
     *  <body> = <definition>* <sequence>
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(body)
    {
      auto is_definition = [&](auto const& form)
      {
        try
        {
          compile(the_expression_is, form, syntactic_environment, frames, continuation);
          return false;
        }
        catch (const syntax_error<internal_definition_tag>&)
        {
          return true;
        }
      };

      auto sweep = [&](auto const& form)
      {
        let binding_specs = unit;

        for (auto iter = std::cbegin(form); iter != std::cend(form); ++iter)
        {
          if (is_definition(*iter))
          {
            binding_specs = cons(cdr(*iter), binding_specs);
          }
          else
          {
            return std::make_pair(reverse(binding_specs), iter);
          }
        }

        return std::make_pair(reverse(binding_specs), std::end(form));
      };

      auto letrec = [&](auto const& binding_specs, auto const& tail_body)
      {
        // std::cout << "\n"
        //           << "; compiler\t; letrec\n"
        //           << ";\t\t; binding-specs = " << binding_specs << "\n"
        //           << ";\t\t; tail-body = " << tail_body << std::endl;

        let const variables = map(
          [](let const& x)
          {
            return car(x).is<pair>() ? caar(x) : car(x);
          }, binding_specs);
        // std::cout << ";\t\t; variables = " << variables << std::endl;

        let const inits = make_list(length(variables), undefined);
        // std::cout << ";\t\t; inits = " << inits << std::endl;

        let const head_body = map(
          [this](auto&& x)
          {
            if (car(x).template is<pair>())
            {
              return list(intern("set!"),
                          caar(x),
                          cons(intern("lambda"), cdar(x), cdr(x)));
            }
            else
            {
              return cons(intern("set!"), x);
            }
          }, binding_specs);

        // std::cout << ";\t\t; head_body length is " << length(head_body) << std::endl;
        //
        // for (const auto& each : head_body)
        // {
        //   std::cout << ";\t\t; " << each << std::endl;
        // }

        let const result = cons(cons(intern("lambda"), // XXX NOT HYGIENIC!!!
                                     variables,
                                     append(head_body, tail_body)),
                                inits);

        // std::cout << "\t\t; result = " << result << std::endl;

        return result;
      };

      if (cdr(expression).is<null>()) // is tail-sequence
      {
        return compile(in_a_tail_context.take_over(the_expression_is),
                       car(expression),
                       syntactic_environment,
                       frames,
                       continuation);
      }
      else if (auto const [binding_specs, tail_body] = sweep(expression); binding_specs)
      {
        return compile(the_expression_is,
                       letrec(binding_specs, tail_body),
                       syntactic_environment,
                       frames,
                       continuation);
      }
      else
      {
        return compile(the_expression_is,
                       car(expression),
                       syntactic_environment,
                       frames,
                       cons(make<instruction>(mnemonic::DROP),
                            sequence(the_expression_is,
                                     cdr(expression),
                                     syntactic_environment,
                                     frames,
                                     continuation)));
      }
    }

    /* ---- Operand ------------------------------------------------------------
     *
     *  <operand> = <expression>
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(operand)
    {
      if (expression.is<pair>())
      {
        return operand(in_context_free,
                       cdr(expression),
                       syntactic_environment,
                       frames,
                       compile(in_context_free,
                               car(expression),
                               syntactic_environment,
                               frames,
                               cons(make<instruction>(mnemonic::CONS), continuation)));
      }
      else
      {
        return compile(in_context_free,
                       expression,
                       syntactic_environment,
                       frames,
                       continuation);
      }
    }

    /* ---- Conditional --------------------------------------------------------
     *
     *  <conditional> = (if <test> <consequent> <alternate>)
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(conditional)
    {
      debug(car(expression), faint, " ; is <test>");

      if (the_expression_is.in_a_tail_context())
      {
        auto&& consequent =
          compile(in_a_tail_context,
                  cadr(expression),
                  syntactic_environment,
                  frames,
                  list(make<instruction>(mnemonic::RETURN)));

        auto&& alternate =
          cddr(expression)
            ? compile(in_a_tail_context,
                      caddr(expression),
                      syntactic_environment,
                      frames,
                      list(make<instruction>(mnemonic::RETURN)))
            : list(make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                   make<instruction>(mnemonic::RETURN));

        return compile(in_context_free,
                       car(expression), // <test>
                       syntactic_environment,
                       frames,
                       cons(make<instruction>(mnemonic::TAIL_SELECT),
                            std::move(consequent),
                            std::move(alternate),
                            cdr(continuation)));
      }
      else
      {
        auto&& consequent =
          compile(in_context_free,
                  cadr(expression),
                  syntactic_environment,
                  frames,
                  list(make<instruction>(mnemonic::JOIN)));

        auto&& alternate =
          cddr(expression)
            ? compile(in_context_free,
                      caddr(expression),
                      syntactic_environment,
                      frames,
                      list(make<instruction>(mnemonic::JOIN)))
            : list(make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                   make<instruction>(mnemonic::JOIN));

        return compile(in_context_free,
                       car(expression), // <test>
                       syntactic_environment,
                       frames,
                       cons(make<instruction>(mnemonic::SELECT), std::move(consequent),
                                                                 std::move(alternate),
                            continuation));
      }
    }

    /* ---- Lambda Expression --------------------------------------------------
     *
     * <lambda expression> = (lambda <formals> <body>)
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(lambda)
    {
      debug(car(expression), faint, " ; is <formals>");

      return cons(make<instruction>(mnemonic::LOAD_CLOSURE),
                  body(the_expression_is,
                       cdr(expression),
                       syntactic_environment,
                       cons(car(expression), frames),
                       list(make<instruction>(mnemonic::RETURN))),
                  continuation);
    }

    /* ---- Call-With-Current-Continuation -------------------------------------
     *
     *  TODO documentation
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(call_cc)
    {
      debug(car(expression), faint, " ; is <procedure>");

      return cons(make<instruction>(mnemonic::LOAD_CONTINUATION),
                  continuation,
                  compile(the_expression_is,
                          car(expression),
                          syntactic_environment,
                          frames,
                          cons(make<instruction>(mnemonic::CALL), continuation)));
    }

    /* ---- Fork ---------------------------------------------------------------
     *
     *  TODO documentation
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(fork)
    {
      debug(car(expression), faint, " ; is <subprogram>");
      return cons(make<instruction>(mnemonic::FORK), cons(car(expression), frames), continuation);
    }

    /* ---- Assignment ---------------------------------------------------------
     *
     *  TODO documentation
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(assignment)
    {
      if (expression.is<null>())
      {
        throw syntax_error<void>("set!");
      }
      else if (de_bruijn_index index { car(expression), frames }; not index.is<null>())
      {
        if (index.is_variadic())
        {
          debug(car(expression), faint, " ; is <variadic bound variable> references ", reset, index);

          return compile(in_context_free,
                         cadr(expression),
                         syntactic_environment,
                         frames,
                         cons(make<instruction>(mnemonic::STORE_VARIADIC), index, continuation));
        }
        else
        {
          debug(car(expression), faint, "; is a <bound variable> references ", reset, index);

          return compile(in_context_free,
                         cadr(expression),
                         syntactic_environment,
                         frames,
                         cons(make<instruction>(mnemonic::STORE_LOCAL), index, continuation));
        }
      }
      else
      {
        debug(car(expression), faint, "; is a <glocal variable>");

        // TODO if (the_expression_is.at_the_top_level) => compile to DEFINE
        //
        // TODO if unbound variable => throw error("it would be an error to perform a set! on an unbound variable (R7RS 5.3.1. Top level definitions)");

        let const g = global(car(expression), syntactic_environment);

        return compile(in_context_free,
                       cadr(expression),
                       syntactic_environment,
                       frames,
                       cons(make<instruction>(mnemonic::STORE_GLOBAL), g, continuation));
      }
    }

    /* ---- Explicit Variable Reference ----------------------------------------
     *
     *  TODO DEPRECATED
     *  TODO REMOVE AFTER IMPLEMENTED MODULE SYSTEM
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(reference)
    {
      if (expression.is<null>())
      {
        debug(car(expression), faint, " ; is <identifier> of itself");
        return unit;
      }
      else if (de_bruijn_index<equivalence_comparator<2>> variable { car(expression), frames }; variable)
      {
        if (variable.is_variadic())
        {
          debug(car(expression), faint, " ; is <identifier> of local variadic ", reset, variable);
          return cons(make<instruction>(mnemonic::LOAD_VARIADIC), variable, continuation);
        }
        else
        {
          debug(car(expression), faint, " ; is <identifier> of local ", reset, variable);
          return cons(make<instruction>(mnemonic::LOAD_LOCAL), variable, continuation);
        }
      }
      else
      {
        debug(car(expression), faint, " ; is <identifier> of glocal variable");
        return cons(make<instruction>(mnemonic::LOAD_GLOBAL), global(car(expression), syntactic_environment), continuation);
      }
    }

    /* ---- Construct ----------------------------------------------------------
     *
     *  This primitive expression type is not currently in use. The procedure
     *  cons is not a primitive expression type and must be redefined as
     *
     *    (define cons
     *      (lambda (a b)
     *        (cons a b)))
     *
     * ---------------------------------------------------------------------- */
    SYNTAX(construct)
    {
      return compile(in_context_free,
                     cadr(expression),
                     syntactic_environment,
                     frames,
                     compile(in_context_free,
                             car(expression),
                             syntactic_environment,
                             frames,
                             cons(make<instruction>(mnemonic::CONS), continuation)));
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
