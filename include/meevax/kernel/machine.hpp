#ifndef INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
#define INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/de_brujin_index.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/procedure.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/syntactic_keyword.hpp>
#include <meevax/kernel/syntax.hpp>
#include <meevax/string/header.hpp>

namespace meevax
{
inline namespace kernel
{
  #define WRITE_DEBUG(...)                                                     \
  if (in_debug_mode())                                                         \
  {                                                                            \
    write_to(standard_debug_port(), header(__func__), indent(), __VA_ARGS__, "\n"); \
  } indent()

  template <typename SK>
  class machine // TR-SECD machine.
  {
    friend SK;

    machine()
    {}

    IMPORT(SK, evaluate, NIL);
    IMPORT(SK, global_environment, NIL);
    IMPORT(SK, in_debug_mode, const);
    IMPORT(SK, in_trace_mode, const);
    IMPORT(SK, intern, NIL);
    IMPORT(SK, standard_debug_port, const);
    IMPORT(SK, standard_error_port, const);
    IMPORT(SK, standard_output_port, const);
    IMPORT(SK, write_to, const);

  protected:
    let s, // stack (holding intermediate results and return address)
        e, // environment (giving values to symbols)
        c, // control (instructions yet to be executed)
        d; // dump (s e c . d)

    /* ---- NOTE ---------------------------------------------------------------
     *
     *  global-environment: g = global_environment()
     *
     *  lexical-environment: e
     *
     *  dynamic-environment: d = (s e c ...)
     *
     *  syntactic-environment: (e . g) when define-syntax invoked
     *
     *  syntactic-continuation: (d . g)
     *
     * ---------------------------------------------------------------------- */

  public:
    // TODO MOVE INTO SK
    template <typename... Ts>
    let const& define(let const& variable, Ts&&... expression)
    {
      push(global_environment(), cons(variable, std::forward<decltype(expression)>(expression)...));

      WRITE_DEBUG(caar(global_environment()), faint, " binds ", reset,
                  cdar(global_environment()));

      return unspecified;
    }

    /* ---- NOTE ---------------------------------------------------------------
     *
     *  This function extends the given syntax environment 'g'. Since the order
     *  of operand evaluation in C ++ is undefined, be aware of the execution
     *  timing of side effects of this function.
     *
     * ---------------------------------------------------------------------- */
    let const locate(let const& x, let & g)
    {
      if (let const binding = assq(x, g); eq(binding, f) /* or cdr(binding).is<keyword>() */) // TODO
      {
        /* ---- R7RS 5.3.1. Top level definitions ------------------------------
         *
         *  At the outermost level of a program, a definition
         *
         *      (define <variable> <expression>)
         *
         *  has essentially the same effect as the assignment expression
         *
         *      (set! <variable> <expression>)
         *
         *  if <variable> is bound to a non-syntax value. However, if
         *  <variable> is not bound, or is a syntactic keyword, then the
         *  definition will bind <variable> to a new location before performing
         *  the assignment, whereas it would be an error to perform a set! on
         *  an unbound variable.
         *
         * ------------------------------------------------------------------ */
        return locate(x, push(g, cons(x, make<syntactic_keyword>(x, g))));
      }
      else
      {
        return binding;
      }
    }

    auto current_continuation() const
    {
      return make<continuation>(s, cons(e, cadr(c), d));
    }

    /* ---- R7RS 4. Expressions ------------------------------------------------
     *
     *  <expression> = <identifier>
     *               | <literal>
     *               | <procedure call>
     *               | <lambda expression>
     *               | <conditional>
     *               | <assignment>
     *               | <derived expression>
     *
     *  Expression types are categorized as primitive or derived. Primitive
     *  expression types include variables and procedure calls. Derived
     *  expression types are not semantically primitive, but can instead be
     *  defined as macros. Suitable syntax definitions of some of the derived
     *  expressions are given in section 7.3.
     *
     *  The procedures force, promise?, make-promise, and make-parameter are
     *  also described in this chapter because they are intimately associated
     *  with the delay, delay-force, and parameterize expression types.
     *
     * ---------------------------------------------------------------------- */
    let compile(
      syntactic_context const& the_expression_is,
      let      & syntactic_environment,
      let const& expression,
      let const& frames = unit,
      let const& continuation = list(make<instruction>(mnemonic::STOP)))
    {
      if (expression.is<null>())
      {
        /* ---- R7RS 4.1.3. Procedure calls ------------------------------------
         *
         *  (<operator> <operand 1> ...)                                 syntax
         *
         *  Note: In many dialects of Lisp, the empty list, (), is a legitimate
         *  expression evaluating to itself. In Scheme, it is an error.
         *
         * ------------------------------------------------------------------ */
        return cons(make<instruction>(mnemonic::LOAD_CONSTANT), unit, continuation);
      }
      else if (not expression.is<pair>()) // is <identifier>
      {
        if (expression.is<symbol>() or expression.is<syntactic_keyword>())
        {
          /* ---- R7RS 4.1.1. Variable references ------------------------------
           *
           *  <variable>                                                 syntax
           *
           *  An expression consisting of a variable (section 3.1) is a
           *  variable reference. The value of the variable reference is the
           *  value stored in the location to which the variable is bound. It
           *  is an error to reference an unbound variable.
           *
           * ------------------------------------------------------------------ */
          if (de_bruijn_index index { expression, frames }; index)
          {
            if (index.is_variadic())
            {
              WRITE_DEBUG(expression, faint, " ; is a <variadic bound variable> references ", reset, index);
              return cons(make<instruction>(mnemonic::LOAD_VARIADIC), index, continuation);
            }
            else
            {
              WRITE_DEBUG(expression, faint, " ; is a <bound variable> references ", reset, index);
              return cons(make<instruction>(mnemonic::LOAD_LOCAL), index, continuation);
            }
          }
          else if (expression.is<syntactic_keyword>())
          {
            WRITE_DEBUG(expression, faint, " ; is <syntactic-keyword>");
            return cons(make<instruction>(mnemonic::STRIP), expression, continuation);
          }
          else
          {
            WRITE_DEBUG(expression, faint, " ; is a <free variable>");
            return cons(make<instruction>(mnemonic::LOAD_GLOBAL), locate(expression, syntactic_environment), continuation);
          }
        }
        else // is <self-evaluating>
        {
          WRITE_DEBUG(expression, faint, " ; is <self-evaluating>");
          return cons(make<instruction>(mnemonic::LOAD_CONSTANT), expression, continuation);
        }
      }
      else // is (applicant . arguments)
      {
        if (let const& applicant = lookup(car(expression), syntactic_environment); not de_bruijn_index(car(expression), frames))
        {
          if (applicant.is<syntax>())
          {
            WRITE_DEBUG(magenta, "(", reset, car(expression), faint, " ; is <primitive expression>") >> indent::width;

            let result =
              applicant.as<syntax>().compile(
                the_expression_is, syntactic_environment, cdr(expression), frames, continuation);

            WRITE_DEBUG(magenta, ")") << indent::width;

            return result;
          }
          else if (applicant.is<SK>())
          {
            WRITE_DEBUG(magenta, "(", reset, car(expression), faint, " ; is <macro application>");

            let const result = applicant.as<SK>().macroexpand(applicant, expression);

            WRITE_DEBUG(result);

            return compile(in_context_free, syntactic_environment, result, frames, continuation);
          }
        }

        /* ---- R7RS 4.1.3. Procedure calls ------------------------------------
         *
         *  (<operator> <operand 1> ...)                                 syntax
         *
         *  A procedure call is written by enclosing in parentheses an
         *  expression for the procedure to be called followed by expressions
         *  for the arguments to be passed to it. The operator and operand
         *  expressions are evaluated (in an unspecified order) and the
         *  resulting procedure is passed the resulting arguments.
         *
         *  The procedures in this document are available as the values of
         *  variables exported by the standard libraries. For example, the
         *  addition and multiplication procedures in the above examples are
         *  the values of the variables + and * in the base library. New
         *  procedures are created by evaluating lambda expressions (see
         *  section 4.1.4).
         *
         *  Procedure calls can return any number of values (see values in
         *  section 6.10). Most of the procedures defined in this report return
         *  one value or, for procedures such as apply, pass on the values
         *  returned by a call to one of their arguments. Exceptions are noted
         *  in the individual descriptions.
         *
         *  Note: In contrast to other dialects of Lisp, the order of
         *  evaluation is unspecified, and the operator expression and the
         *  operand expressions are always evaluated with the same evaluation
         *  rules.
         *
         *  Note: Although the order of evaluation is otherwise unspecified,
         *  the effect of any concurrent evaluation of the operator and operand
         *  expressions is constrained to be consistent with some sequential
         *  order of evaluation. The order of evaluation may be chosen
         *  differently for each procedure call.
         *
         *  Note: In many dialects of Lisp, the empty list, (), is a legitimate
         *  expression evaluating to itself. In Scheme, it is an error.
         *
         * ------------------------------------------------------------------ */

        WRITE_DEBUG(magenta, "(", reset, faint, " ; is <procedure call>") >> indent::width;

        let const result =
          operand(in_context_free,
                  syntactic_environment,
                  cdr(expression),
                  frames,
                  compile(in_context_free,
                          syntactic_environment,
                          car(expression),
                          frames,
                          cons(make<instruction>(the_expression_is.in_a_tail_context() ? mnemonic::TAIL_CALL
                                                                                       : mnemonic::     CALL),
                               continuation)));

        WRITE_DEBUG(magenta, ")") << indent::width;

        return result;
      }
    }

    template <bool Trace = false>
    let execute()
    {
    dispatch:
      if constexpr (Trace)
      {
        std::cerr << faint << header("trace s") << reset <<  s << "\n"
                  << faint << header("      e") << reset <<  e << "\n"
                  << faint << header("      c") << reset <<  c << "\n"
                  << faint << header("      d") << reset <<  d << "\n" << std::endl;
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
        *    i = (caadr c)
        *    j = (cdadr c)
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
        push(s, cadr(c));
        c = cddr(c);
        goto dispatch;

      // case mnemonic::LOAD_SYNTAX: /* -------------------------------------------
      //   *
      //   *                 S  E (LOAD-SYNTAX syntax . C) D
      //   *  => (constant . S) E                       C  D
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
        *             S  E (STRIP identifier . C) D
        *  => (form . S) E                     C  D
        *
        * ------------------------------------------------------------------- */
        push(s, cadr(c).template as<syntactic_keyword>().lookup());
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
        *                       s  e (LDK cc . c) d
        *  => ((continuation) . s) e           c  d
        *
        *  where continuation = (s e c . d)
        *
        * ------------------------------------------------------------------- */
        push(s, list(current_continuation()));
        c = cddr(c);
        goto dispatch;

      case mnemonic::FORK: /* --------------------------------------------------
        *
        *          s  e (FORK k . c) d
        *  => (p . s) e           c  d
        *
        *  where k = (<program declaration> . <frames>)
        *
        * ------------------------------------------------------------------- */
        push(s, make<SK>(current_continuation(), global_environment()));
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
        c = car(s).template is<null>() or (car(s) != f) ? cadr(c) : caddr(c);
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
        *     (x . S) E (DEFINE cell . C) D
        *  => (x . S) E                C  D
        *
        *  where cell = (identifier . <unknown>)
        *
        * ------------------------------------------------------------------- */
        cdadr(c) = car(s);
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
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *     (k operands . s)  e (CALL . c) d
        *  =>   (operand  . s') e'        c' d'
        *
        *  where k = (s' e' c' . 'd)
        *
        * ------------------------------------------------------------------- */
        {
          s = cons(caadr(s), callee.as<continuation>().s());
          e =                callee.as<continuation>().e();
          c =                callee.as<continuation>().c();
          d =                callee.as<continuation>().d();
        }
        else
        {
          throw error(make<string>("not applicable"), callee);
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
          s = cons(caadr(s), callee.as<continuation>().s());
          e =                callee.as<continuation>().e();
          c =                callee.as<continuation>().c();
          d =                callee.as<continuation>().d();
        }
        else
        {
          throw error(make<string>("not applicable"), callee);
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
        *    (result . S) E (DROP . C) D
        *  =>          S  E         C  D
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
        if (let const& binding = cadr(c); cdr(binding).is<null>())
        {
          cdr(binding) = car(s);
        }
        else
        {
          cdr(binding).store(car(s));
        }
        c = cddr(c);
        goto dispatch;

      case mnemonic::STORE_LOCAL: /* -------------------------------------------
        *
        *     (value . S) E (STORE-LOCAL (i . j) . C) D
        *  => (value . S) E                        C  D
        *
        * ------------------------------------------------------------------- */
        car(list_tail(list_ref(e, caadr(c)), cdadr(c))).store(car(s));
        c = cddr(c);
        goto dispatch;

      case mnemonic::STORE_VARIADIC:
        cdr(list_tail(list_ref(e, caadr(c)), cdadr(c))).store(car(s));
        c = cddr(c);
        goto dispatch;

      default: // ERROR
      case mnemonic::STOP: /* --------------------------------------------------
        *
        *     (result . S) E (STOP . C) D
        *  =>           S  E         C  D
        *
        * ------------------------------------------------------------------- */
        c = cdr(c);
        return pop(s); // return car(s);
      }
    }

  protected: // Primitive Expression Types
    SYNTAX(quotation) /* -------------------------------------------------------
    *
    *  (quote <datum>)                                                   syntax
    *
    *  (quote <datum>) evaluates to <datum>. <Datum> can be any external
    *  representation of a Scheme object (see section 3.3). This notation is
    *  used to include literal constants in Scheme code.
    *
    *  (quote <datum>) can be abbreviated as '<datum>. The two notations are
    *  equivalent in all respects.
    *
    *  Numerical constants, string constants, character constants, vector
    *  constants, bytevector constants, and boolean constants evaluate to
    *  themselves; they need not be quoted.
    *
    *  As noted in section 3.4, it is an error to attempt to alter a constant
    *  (i.e. the value of a literal expression) using a mutation procedure like
    *  set-car! or string-set!.
    *
    * ----------------------------------------------------------------------- */
    {
      WRITE_DEBUG(car(expression), faint, " ; is <datum>");
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
                         syntactic_environment,
                         car(expression),
                         frames,
                         continuation);
        }
        else
        {
          return compile(at_the_top_level,
                         syntactic_environment,
                         car(expression),
                         frames,
                         cons(make<instruction>(mnemonic::DROP),
                              sequence(at_the_top_level,
                                       syntactic_environment,
                                       cdr(expression),
                                       frames,
                                       continuation)));
        }
      }
      else
      {
        if (cdr(expression).is<null>()) // is tail sequence
        {
          return compile(the_expression_is,
                         syntactic_environment,
                         car(expression),
                         frames,
                         continuation);
        }
        else
        {
          return compile(in_context_free,
                         syntactic_environment,
                         car(expression), // head expression
                         frames,
                         cons(make<instruction>(mnemonic::DROP), // pop result of head expression
                              sequence(in_context_free,
                                       syntactic_environment,
                                       cdr(expression), // rest expressions
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
        WRITE_DEBUG(car(expression), faint, " ; is <variable>");

        if (car(expression).is<pair>()) // (define (f . <formals>) <body>)
        {
          let const g = locate(caar(expression), syntactic_environment);

          return compile(in_context_free,
                         syntactic_environment,
                         cons(intern("lambda"), cdar(expression), cdr(expression)),
                         frames,
                         cons(make<instruction>(mnemonic::DEFINE), g, continuation));
        }
        else // (define x ...)
        {
          let const g = locate(car(expression), syntactic_environment);

          return compile(in_context_free,
                         syntactic_environment,
                         cdr(expression) ? cadr(expression) : unspecified,
                         frames,
                         cons(make<instruction>(mnemonic::DEFINE), g,
                              continuation));
        }
      }
      else
      {
        indent() << indent::width; // XXX DIRTY HACK!
        throw tagged_syntax_error<internal_definition_tag>(
          make<string>("definition cannot appear in this context"), unit);
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
          compile(the_expression_is, syntactic_environment, form, frames, continuation);
          return false;
        }
        catch (const tagged_syntax_error<internal_definition_tag>&)
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
                       syntactic_environment,
                       car(expression),
                       frames,
                       continuation);
      }
      else if (auto const [binding_specs, tail_body] = sweep(expression); binding_specs)
      {
        return compile(the_expression_is,
                       syntactic_environment,
                       letrec(binding_specs, tail_body),
                       frames,
                       continuation);
      }
      else
      {
        return compile(the_expression_is,
                       syntactic_environment,
                       car(expression),
                       frames,
                       cons(make<instruction>(mnemonic::DROP),
                            sequence(the_expression_is,
                                     syntactic_environment,
                                     cdr(expression),
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
                       syntactic_environment,
                       cdr(expression),
                       frames,
                       compile(in_context_free,
                               syntactic_environment,
                               car(expression),
                               frames,
                               cons(make<instruction>(mnemonic::CONS), continuation)));
      }
      else
      {
        return compile(in_context_free,
                       syntactic_environment,
                       expression,
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
      WRITE_DEBUG(car(expression), faint, " ; is <test>");

      if (the_expression_is.in_a_tail_context())
      {
        auto consequent =
          compile(in_a_tail_context,
                  syntactic_environment,
                  cadr(expression),
                  frames,
                  list(make<instruction>(mnemonic::RETURN)));

        auto alternate =
          cddr(expression)
            ? compile(in_a_tail_context,
                      syntactic_environment,
                      caddr(expression),
                      frames,
                      list(make<instruction>(mnemonic::RETURN)))
            : list(make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                   make<instruction>(mnemonic::RETURN));

        return compile(in_context_free,
                       syntactic_environment,
                       car(expression), // <test>
                       frames,
                       cons(make<instruction>(mnemonic::TAIL_SELECT), consequent, alternate,
                            cdr(continuation)));
      }
      else
      {
        auto consequent =
          compile(in_context_free,
                  syntactic_environment,
                  cadr(expression),
                  frames,
                  list(make<instruction>(mnemonic::JOIN)));

        auto alternate =
          cddr(expression)
            ? compile(in_context_free,
                      syntactic_environment,
                      caddr(expression),
                      frames,
                      list(make<instruction>(mnemonic::JOIN)))
            : list(make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                   make<instruction>(mnemonic::JOIN));

        return compile(in_context_free,
                       syntactic_environment,
                       car(expression), // <test>
                       frames,
                       cons(make<instruction>(mnemonic::SELECT), consequent, alternate,
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
      WRITE_DEBUG(car(expression), faint, " ; is <formals>");

      return cons(make<instruction>(mnemonic::LOAD_CLOSURE),
                  body(the_expression_is,
                       syntactic_environment,
                       cdr(expression),
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
      WRITE_DEBUG(car(expression), faint, " ; is <procedure>");

      return cons(make<instruction>(mnemonic::LOAD_CONTINUATION),
                  continuation,
                  compile(the_expression_is,
                          syntactic_environment,
                          car(expression),
                          frames,
                          cons(make<instruction>(mnemonic::CALL), continuation)));
    }

    SYNTAX(fork) /* ------------------------------------------------------------
    *
    *  (fork-with-current-syntactic-continuation <program>)              syntax
    *
    *  Semantics: The syntax fork-with-current-syntactic-continuation packages
    *  the given <program> definition and the continuation of the current
    *  compilation as a "subprogram".
    *
    * ----------------------------------------------------------------------- */
    {
      WRITE_DEBUG(car(expression), faint, " ; is <subprogram>");
      return cons(make<instruction>(mnemonic::FORK), cons(car(expression), frames), continuation);
    }

    SYNTAX(assignment) /* ------------------------------------------------------
    *
    *  (set! <variable> <expression>)                                    syntax
    *
    *  Semantics: <Expression> is evaluated, and the resulting value is stored
    *  in the location to which <variable> is bound. It is an error if
    *  <variable> is not bound either in some region enclosing the set!
    *  expression or else globally. The result of the set! expression is
    *  unspecified.
    *
    * ----------------------------------------------------------------------- */
    {
      if (expression.is<null>())
      {
        throw syntax_error(make<string>("set!"), unit);
      }
      else if (de_bruijn_index index { car(expression), frames }; not index.is<null>())
      {
        if (index.is_variadic())
        {
          WRITE_DEBUG(car(expression), faint, " ; is <variadic bound variable> references ", reset, index);

          return compile(in_context_free,
                         syntactic_environment,
                         cadr(expression),
                         frames,
                         cons(make<instruction>(mnemonic::STORE_VARIADIC), index, continuation));
        }
        else
        {
          WRITE_DEBUG(car(expression), faint, "; is a <bound variable> references ", reset, index);

          return compile(in_context_free,
                         syntactic_environment,
                         cadr(expression),
                         frames,
                         cons(make<instruction>(mnemonic::STORE_LOCAL), index, continuation));
        }
      }
      else
      {
        WRITE_DEBUG(car(expression), faint, "; is a <free variable>");

        let const g = locate(car(expression), syntactic_environment);

        if (the_expression_is.at_the_top_level() and cdr(g).is<syntactic_keyword>())
        {
          throw syntax_error(
            make<string>("set!: it would be an error to perform a set! on an unbound variable (R7RS 5.3.1)"),
            unit);
        }
        else
        {
          return compile(in_context_free,
                         syntactic_environment,
                         cadr(expression),
                         frames,
                         cons(make<instruction>(mnemonic::STORE_GLOBAL), g, continuation));
        }
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
        WRITE_DEBUG(car(expression), faint, " ; is <identifier> of itself");
        return unit;
      }
      else if (de_bruijn_index<equivalence_comparator<2>> variable { car(expression), frames }; variable)
      {
        if (variable.is_variadic())
        {
          WRITE_DEBUG(car(expression), faint, " ; is <identifier> of local variadic ", reset, variable);
          return cons(make<instruction>(mnemonic::LOAD_VARIADIC), variable, continuation);
        }
        else
        {
          WRITE_DEBUG(car(expression), faint, " ; is <identifier> of local ", reset, variable);
          return cons(make<instruction>(mnemonic::LOAD_LOCAL), variable, continuation);
        }
      }
      else
      {
        WRITE_DEBUG(car(expression), faint, " ; is <identifier> of free variable");
        return cons(make<instruction>(mnemonic::LOAD_GLOBAL), locate(car(expression), syntactic_environment), continuation);
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
                     syntactic_environment,
                     cadr(expression),
                     frames,
                     compile(in_context_free,
                             syntactic_environment,
                             car(expression),
                             frames,
                             cons(make<instruction>(mnemonic::CONS), continuation)));
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
