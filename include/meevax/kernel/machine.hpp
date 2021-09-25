/*
   Copyright 2018-2021 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
#define INCLUDED_MEEVAX_KERNEL_MACHINE_HPP

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/de_bruijn_index.hpp>
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
  #define WRITE_DEBUG(...)                                                     \
  if (current_syntactic_continuation.is_debug_mode())                          \
  {                                                                            \
    current_syntactic_continuation.write_to(                                   \
      current_syntactic_continuation.standard_debug_port(), header(__func__), indent(), __VA_ARGS__, "\n"); \
  } indent()

  enum class execution_context
  {
    none,

    trace = (1 << 0),

    size,
  };

  template <typename SK>
  class machine // TR-SECD machine.
  {
    friend SK;

    using syntactic_continuation = SK; // HACK

    machine()
    {}

    IMPORT(SK, fork, NIL);
    IMPORT(SK, is_trace_mode, const);
    IMPORT(SK, locate, NIL);

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
    static auto compile(
      syntactic_context const current_syntactic_context,
      syntactic_continuation & current_syntactic_continuation,
      pair::const_reference expression,
      pair::const_reference frames = unit,
      pair::const_reference continuation = list(make<instruction>(mnemonic::STOP))) -> pair::value_type
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
        if (expression.is<symbol>() or expression.is<identifier>())
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
          if (auto const variable = de_bruijn_index(expression, frames); variable.is_bound())
          {
            if (variable.is_variadic)
            {
              WRITE_DEBUG(expression, faint, " ; is a <variadic bound variable> references ", reset, variable.index);
              return cons(make<instruction>(mnemonic::LOAD_VARIADIC), variable.index,
                          continuation);
            }
            else
            {
              WRITE_DEBUG(expression, faint, " ; is a <bound variable> references ", reset, variable.index);
              return cons(make<instruction>(mnemonic::LOAD_LOCAL), variable.index,
                          continuation);
            }
          }
          else
          {
            WRITE_DEBUG(expression, faint, " ; is a <free variable>");
            return cons(make<instruction>(mnemonic::LOAD_GLOBAL), current_syntactic_continuation.locate(expression),
                        continuation);
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
        if (let const& applicant = current_syntactic_continuation.lookup(car(expression)); de_bruijn_index(car(expression), frames).is_free())
        {
          if (applicant.is<syntax>())
          {
            WRITE_DEBUG(magenta, "(", reset, car(expression), faint, " ; is <primitive expression>") >> indent::width;

            let const result =
              applicant.as<syntax>().compile(
                current_syntactic_context, current_syntactic_continuation, cdr(expression), frames, continuation);

            WRITE_DEBUG(magenta, ")") << indent::width;

            return result;
          }
          else if (applicant.is<SK>())
          {
            WRITE_DEBUG(magenta, "(", reset, car(expression), faint, " ; is <macro application>");

            let const result = applicant.as<SK>().macroexpand(applicant, expression);

            WRITE_DEBUG(result);

            return compile(context::none, current_syntactic_continuation, result, frames, continuation);
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
          operand(context::none,
                  current_syntactic_continuation,
                  cdr(expression),
                  frames,
                  compile(context::none,
                          current_syntactic_continuation,
                          car(expression),
                          frames,
                          cons(make<instruction>(static_cast<bool>(current_syntactic_context bitand context::tail) ? mnemonic::TAIL_CALL : mnemonic::CALL),
                               continuation)));

        WRITE_DEBUG(magenta, ")") << indent::width;

        return result;
      }
    }

    inline auto current_continuation() const -> pair::value_type
    {
      return make<continuation>(s, cons(e, cadr(c), d));
    }

    template <auto Context = execution_context::none>
    inline auto execute() -> pair::value_type
    {
    decode:
      if constexpr (static_cast<bool>(Context bitand execution_context::trace))
      {
        std::cerr << faint << header("trace s") << reset << s << "\n"
                  << faint << header("      e") << reset << e << "\n"
                  << faint << header("      c") << reset << c << "\n"
                  << faint << header("      d") << reset << d << "\n" << std::endl;
      }

      switch (car(c).template as<instruction>().value)
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
        s = cons(list_ref(list_ref(e, caadr(c)), cdadr(c)), s);
        c = cddr(c);
        goto decode;

      case mnemonic::LOAD_VARIADIC: /* -----------------------------------------
        *
        *               S  E (LOAD-VARIADIC (i . j) . C) D
        *  => (result . S) E                          C  D
        *
        *  where result = (list-tail (list-ref E i) j)
        *
        * ------------------------------------------------------------------- */
        s = cons(list_tail(list_ref(e, caadr(c)), cdadr(c)), s);
        c = cddr(c);
        goto decode;

      case mnemonic::LOAD_CONSTANT: /* -----------------------------------------
        *
        *                 S  E (LOAD-CONSTANT constant . C) D
        *  => (constant . S) E                           C  D
        *
        * ------------------------------------------------------------------- */
        s = cons(cadr(c), s);
        c = cddr(c);
        goto decode;

      case mnemonic::LOAD_GLOBAL: /* -------------------------------------------
        *
        *               S  E (LOAD-GLOBAL <identifier> . C) D
        *  => (object . S) E                             C  D
        *
        *  where <identifier> = (<symbol> . <unknown>)
        *
        * ------------------------------------------------------------------- */
        s = cons(cdadr(c), s);
        c = cddr(c);
        goto decode;

      case mnemonic::LOAD_CLOSURE: /* ------------------------------------------
        *
        *                S  E (LOAD-CLOSURE body . C) D
        *  => (closure . S) E                      C  D
        *
        * ------------------------------------------------------------------- */
        s = cons(make<closure>(cadr(c), e), s);
        c = cddr(c);
        goto decode;

      case mnemonic::LOAD_CONTINUATION: /* -------------------------------------
        *
        *                       s  e (LDK cc . c) d
        *  => ((continuation) . s) e           c  d
        *
        *  where continuation = (s e c . d)
        *
        * ------------------------------------------------------------------- */
        s = cons(list(current_continuation()), s);
        c = cddr(c);
        goto decode;

      case mnemonic::FORK: /* --------------------------------------------------
        *
        *          s  e (FORK k . c) d
        *  => (p . s) e           c  d
        *
        *  where k = (<program declaration> . <frames>)
        *
        * ------------------------------------------------------------------- */
        s = cons(fork(), s);
        c = cddr(c);
        goto decode;

      case mnemonic::SELECT: /* ------------------------------------------------
        *
        *     (test . S) E (SELECT consequent alternate . C)  D
        *  =>         S  E         selection             (C . D)
        *
        *  where selection = (if test consequent alternate)
        *
        * ------------------------------------------------------------------- */
        d = cons(cdddr(c), d);
        [[fallthrough]];

      case mnemonic::TAIL_SELECT: /* -------------------------------------------
        *
        *     (test . S) E (SELECT consequent alternate . C)  D
        *  =>         S  E         selection                  D
        *
        *  where selection = (if test consequent alternate)
        *
        * ------------------------------------------------------------------- */
        c = if_(car(s)) ? cadr(c) : caddr(c);
        s = cdr(s);
        goto decode;

      case mnemonic::JOIN: /* --------------------------------------------------
        *
        *     S E (JOIN) (C . D)
        *  => S E         C   D
        *
        * ------------------------------------------------------------------- */
        c = car(d);
        d = cdr(d);
        goto decode;

      case mnemonic::DEFINE: /* ------------------------------------------------
        *
        *     (x . S) E (DEFINE <identifier> . C) D
        *  => (x . S) E                        C  D
        *
        *  where <identifier> = (<symbol> . <unknown>)
        *
        * ------------------------------------------------------------------- */
        cdadr(c) = car(s);
        c = cddr(c);
        goto decode;

      case mnemonic::CALL:
        if (let const& callee = car(s); callee.is<closure>()) /* ---------------
        *
        *     (<closure> arguments . s)              e (CALL . c)          d
        *  =>                        () (arguments . e')       c' (s e c . d)
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        {
          d = cons(cddr(s), e, cdr(c), d);
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is<procedure>()) /* ------------------------------------
        *
        *     (<procedure> arguments . s) e (CALL . c) d
        *  =>              (<result> . s) e         c  d
        *
        *  where <result> = procedure(arguments)
        *
        * ------------------------------------------------------------------- */
        {
          s = cons(std::invoke(callee.as<procedure>(), cadr(s)), cddr(s));
          c = cdr(c);
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *     (<continuation> arguments . s)  e (CALL . c) d
        *  =>                (arguments . s') e'        c' d'
        *
        *  where <continuation> = (s' e' c' . 'd)
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
        goto decode;

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
        goto decode;

      case mnemonic::DUMMY: /* -------------------------------------------------
        *
        *     s                e (DUMMY . c) d
        *  => s (<undefined> . e)         c  d
        *
        * ------------------------------------------------------------------- */
        e = cons(undefined, e);
        c = cdr(c);
        goto decode;

      case mnemonic::RECURSIVE_CALL: /* ----------------------------------------
        *
        *      (<closure> arguments . s) (<dummy> . e) (RECURSIVE_CALL . c) d
        *  =>  () (set-car! e' arguments) c' (s e c . d)
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        cadar(s) = cadr(s);
        d = cons(cddr(s), cdr(e), cdr(c), d);
        c = caar(s);
        e = cdar(s);
        s = unit;
        goto decode;

      case mnemonic::RETURN: /* ------------------------------------------------
        *
        *     (result . S)  E (RETURN . C) (S' E' C' . D)
        *  => (result . S') E'          C'             D
        *
        * ------------------------------------------------------------------- */
        s = cons(car(s), pop(d));
        e = pop(d);
        c = pop(d);
        goto decode;

      case mnemonic::CONS: /* --------------------------------------------------
        *
        *     ( X   Y  . S) E (CONS . C) D
        *  => ((X . Y) . S) E         C  D
        *
        * ------------------------------------------------------------------- */
        s = cons(cons(car(s), cadr(s)), cddr(s));
        c = cdr(c);
        goto decode;

      case mnemonic::DROP: /* --------------------------------------------------
        *
        *    (result . S) E (DROP . C) D
        *  =>          S  E         C  D
        *
        * ------------------------------------------------------------------- */
        s = cdr(s);
        c = cdr(c);
        goto decode;

      case mnemonic::STORE_GLOBAL: /* ------------------------------------------
        *
        *     (value . S) E (STORE-GLOBAL <identifier> . C) D
        *  => (value . S) E                              C  D
        *
        *  where <identifier> = (<symbol> . x)
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
        goto decode;

      case mnemonic::STORE_LOCAL: /* -------------------------------------------
        *
        *     (value . S) E (STORE-LOCAL (i . j) . C) D
        *  => (value . S) E                        C  D
        *
        * ------------------------------------------------------------------- */
        car(list_tail(list_ref(e, caadr(c)), cdadr(c))).store(car(s));
        c = cddr(c);
        goto decode;

      case mnemonic::STORE_VARIADIC:
        cdr(list_tail(list_ref(e, caadr(c)), cdadr(c))).store(car(s));
        c = cddr(c);
        goto decode;

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

  protected:
    static SYNTAX(quotation) /* ------------------------------------------------
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
      return cons(make<instruction>(mnemonic::LOAD_CONSTANT), car(expression),
                  continuation);
    }

    static SYNTAX(sequence) /* -------------------------------------------------
    *
    *  Both of Scheme's sequencing constructs are named begin, but the two
    *  have slightly different forms and uses:
    *
    *  (begin <expression or definition> ...)                            syntax
    *
    *  This form of begin can appear as part of a <body>, or at the outermost
    *  level of a <program>, or at the REPL, or directly nested in a begin that
    *  is itself of this form. It causes the contained expressions and
    *  definitions to be evaluated exactly as if the enclosing begin construct
    *  were not present.
    *
    *  Rationale: This form is commonly used in the output of macros (see
    *  section 4.3) which need to generate multiple definitions and splice them
    *  into the context in which they are expanded.
    *
    *  (begin <expression 1> <expression 2> ...)                         syntax
    *
    *  This form of begin can be used as an ordinary expression. The
    *  <expression>s are evaluated sequentially from left to right, and the
    *  values of the last <expression> are returned. This expression type is
    *  used to sequence side effects such as assignments or input and output.
    *
    * ---------------------------------------------------------------------- */
    {
      if (static_cast<bool>(current_syntactic_context bitand context::outermost))
      {
        if (cdr(expression).is<null>())
        {
          return compile(current_syntactic_context,
                         current_syntactic_continuation,
                         car(expression),
                         frames,
                         continuation);
        }
        else
        {
          return compile(context::outermost,
                         current_syntactic_continuation,
                         car(expression),
                         frames,
                         cons(make<instruction>(mnemonic::DROP),
                              sequence(context::outermost,
                                       current_syntactic_continuation,
                                       cdr(expression),
                                       frames,
                                       continuation)));
        }
      }
      else
      {
        if (cdr(expression).is<null>()) // is tail sequence
        {
          return compile(current_syntactic_context,
                         current_syntactic_continuation,
                         car(expression),
                         frames,
                         continuation);
        }
        else
        {
          return compile(context::none,
                         current_syntactic_continuation,
                         car(expression), // head expression
                         frames,
                         cons(make<instruction>(mnemonic::DROP), // pop result of head expression
                              sequence(context::none,
                                       current_syntactic_continuation,
                                       cdr(expression), // rest expressions
                                       frames,
                                       continuation)));
        }
      }
    }

    static SYNTAX(definition) /* -----------------------------------------------
    *
    *  A variable definition binds one or more identifiers and specifies an
    *  initial value for each of them. The simplest kind of variable definition
    *  takes one of the following forms:
    *
    *  - (define <variable> <expression>)
    *
    *  - (define (<variable> <formals>) <body>)
    *
    *    <Formals> are either a sequence of zero or more variables, or a
    *    sequence of one or more variables followed by a space-delimited period
    *    and another variable (as in a lambda expression). This form is
    *    equivalent to
    *
    *        (define <variable> (lambda (<formals>) <body>)).
    *
    *  - (define (<variable> . <formal>) <body>)
    *
    *    <Formal> is a single variable. This form is equivalent to
    *
    *        (define <variable> (lambda <formal> <body>)).
    *
    * ----------------------------------------------------------------------- */
    {
      if (frames.is<null>() or static_cast<bool>(current_syntactic_context bitand context::outermost))
      {
        WRITE_DEBUG(car(expression), faint, " ; is <variable>");

        if (car(expression).is<pair>()) // (define (f . <formals>) <body>)
        {
          return compile(context::none,
                         current_syntactic_continuation,
                         cons(make<syntax>("lambda", lambda), cdar(expression), cdr(expression)),
                         frames,
                         cons(make<instruction>(mnemonic::DEFINE), current_syntactic_continuation.locate(caar(expression)),
                              continuation));
        }
        else // (define x ...)
        {
          return compile(context::none,
                         current_syntactic_continuation,
                         cdr(expression) ? cadr(expression) : unspecified,
                         frames,
                         cons(make<instruction>(mnemonic::DEFINE), current_syntactic_continuation.locate(car(expression)),
                              continuation));
        }
      }
      else
      {
        throw syntax_error(make<string>("definition cannot appear in this context"), unit);
      }
    }

    static SYNTAX(letrec) /* ---------------------------------------------------
    *
    *  (letrec <bindings> <body>)                                        syntax
    *
    *  Syntax: <Bindings> has the form
    *
    *      ((<variable 1> <init 1>) ...),
    *
    *  and <body> is a sequence of zero or more definitions followed by one or
    *  more expressions as described in section 4.1.4. It is an error for a
    *  <variable> to appear more than once in the list of variables being bound.
    *
    *  Semantics: The <variable>s are bound to fresh locations holding
    *  unspecified values, the <init>s are evaluated in the resulting
    *  environment (in some unspecified order), each <variable> is assigned to
    *  the result of the corresponding <init>, the <body> is evaluated in the
    *  resulting environment, and the values of the last expression in <body>
    *  are returned. Each binding of a <variable> has the entire letrec
    *  expression as its region, making it possible to define mutually
    *  recursive procedures.
    *
    *      (letrec ((even?
    *                 (lambda (n)
    *                   (if (zero? n) #t
    *                       (odd? (- n 1)))))
    *               (odd?
    *                 (lambda (n)
    *                   (if (zero? n) #f
    *                       (even? (- n 1))))))
    *        (even? 88))
    *                                  => #t
    *
    *  One restriction on letrec is very important: if it is not possible to
    *  evaluate each <init> without assigning or referring to the value of any
    *  <variable>, it is an error. The restriction is necessary because letrec
    *  is defined in terms of a procedure call where a lambda expression binds
    *  the <variable>s to the values of the <init>s. In the most common uses of
    *  letrec, all the <init>s are lambda expressions and the restriction is
    *  satisfied automatically.
    *
    * ----------------------------------------------------------------------- */
    {
      auto const& [bindings, body] = unpair(expression);

      auto const& [variables, inits] = unzip2(bindings);

      return cons(make<instruction>(mnemonic::DUMMY),
                  operand(context::none,
                          current_syntactic_continuation,
                          inits,
                          cons(variables, frames),
                          lambda(context::none,
                                 current_syntactic_continuation,
                                 cons(variables, body),
                                 frames,
                                 cons(make<instruction>(mnemonic::RECURSIVE_CALL),
                                      continuation))));
    }

    static SYNTAX(body)
    {
      auto is_definition = [&](pair::const_reference form)
      {
        if (form.is<pair>() and de_bruijn_index(car(form), frames).is_free())
        {
          let const& callee = current_syntactic_continuation.lookup(car(form));
          return callee.is<syntax>() and callee.as<syntax>().name == "define";
        }
        else
        {
          return false;
        }
      };

      auto sweep = [&](auto const& form)
      {
        let binding_specs = unit;

        for (auto iter = std::begin(form); iter != std::end(form); ++iter)
        {
          if (is_definition(*iter))
          {
            if (cadr(*iter).template is<pair>()) // (define (<variable> . <formals>) <body>)
            {
              auto const& [variable, formals] = unpair(cadr(*iter));

              binding_specs = list(variable, cons(make<syntax>("lambda", lambda), formals, cddr(*iter))) | binding_specs;
            }
            else // (define <variable> <expression>)
            {
              binding_specs = cdr(*iter) | binding_specs;
            }
          }
          else
          {
            return std::make_pair(reverse(binding_specs), iter);
          }
        }

        return std::make_pair(reverse(binding_specs), std::end(form));
      };

      /*
         (lambda <formals> <body>)

         where <body> = <definition>* <expression>* <tail expression>
      */
      if (cdr(expression).is<null>()) // is tail-sequence
      {
        return compile(current_syntactic_context | context::tail,
                       current_syntactic_continuation,
                       car(expression),
                       frames,
                       continuation);
      }
      else if (auto const& [binding_specs, body] = sweep(expression); binding_specs)
      {
        /*
           (letrec* <binding specs> <body>)

               => ((lambda <variables> <assignments> <body>) <initials>)

           where <binding specs> = ((<variable 1> <init 1>) ...)
        */
        return compile(current_syntactic_context,
                       current_syntactic_continuation,
                       cons(cons(make<syntax>("lambda", lambda),
                                 unzip1(binding_specs),
                                 append(map(curry(cons)(make<syntax>("set!", assignment)), binding_specs), body)),
                            make_list(length(binding_specs), undefined)),
                       frames,
                       continuation);
      }
      else
      {
        return compile(current_syntactic_context,
                       current_syntactic_continuation,
                       car(expression),
                       frames,
                       cons(make<instruction>(mnemonic::DROP),
                            sequence(current_syntactic_context,
                                     current_syntactic_continuation,
                                     cdr(expression),
                                     frames,
                                     continuation)));
      }
    }

    static SYNTAX(operand)
    {
      if (expression.is<pair>())
      {
        return operand(context::none,
                       current_syntactic_continuation,
                       cdr(expression),
                       frames,
                       compile(context::none,
                               current_syntactic_continuation,
                               car(expression),
                               frames,
                               cons(make<instruction>(mnemonic::CONS),
                                    continuation)));
      }
      else
      {
        return compile(context::none, current_syntactic_continuation, expression, frames, continuation);
      }
    }

    static SYNTAX(conditional) /* ----------------------------------------------
    *
    *  (if <test> <consequent> <alternate>)                              syntax
    *  (if <test> <consequent>)                                          syntax
    *
    *  Syntax: <Test>, <consequent>, and <alternate> are expressions.
    *
    *  Semantics: An if expression is evaluated as follows: first, <test> is
    *  evaluated. If it yields a true value (see section 6.3), then
    *  <consequent> is evaluated and its values are returned. Otherwise
    *  <alternate> is evaluated and its values are returned. If <test> yields a
    *  false value and no <alternate> is specified, then the result of the
    *  expression is unspecified.
    *
    * ----------------------------------------------------------------------- */
    {
      WRITE_DEBUG(car(expression), faint, " ; is <test>");

      if (static_cast<bool>(current_syntactic_context bitand context::tail))
      {
        auto consequent =
          compile(context::tail,
                  current_syntactic_continuation,
                  cadr(expression),
                  frames,
                  list(make<instruction>(mnemonic::RETURN)));

        auto alternate =
          cddr(expression)
            ? compile(context::tail,
                      current_syntactic_continuation,
                      caddr(expression),
                      frames,
                      list(make<instruction>(mnemonic::RETURN)))
            : list(make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                   make<instruction>(mnemonic::RETURN));

        return compile(context::none,
                       current_syntactic_continuation,
                       car(expression), // <test>
                       frames,
                       cons(make<instruction>(mnemonic::TAIL_SELECT), consequent, alternate,
                            cdr(continuation)));
      }
      else
      {
        auto consequent =
          compile(context::none,
                  current_syntactic_continuation,
                  cadr(expression),
                  frames,
                  list(make<instruction>(mnemonic::JOIN)));

        auto alternate =
          cddr(expression)
            ? compile(context::none,
                      current_syntactic_continuation,
                      caddr(expression),
                      frames,
                      list(make<instruction>(mnemonic::JOIN)))
            : list(make<instruction>(mnemonic::LOAD_CONSTANT), unspecified,
                   make<instruction>(mnemonic::JOIN));

        return compile(context::none,
                       current_syntactic_continuation,
                       car(expression), // <test>
                       frames,
                       cons(make<instruction>(mnemonic::SELECT), consequent, alternate,
                            continuation));
      }
    }

    static SYNTAX(lambda) /* ---------------------------------------------------
    *
    *  (lambda <formals> <body>)                                         syntax
    *
    *  Syntax: <Formals> is a formal arguments list as described below, and
    *  <body> is a sequence of zero or more definitions followed by one or more
    *  expressions.
    *
    *  Semantics: A lambda expression evaluates to a procedure. The environment
    *  in effect when the lambda expression was evaluated is remembered as part
    *  of the procedure. When the procedure is later called with some actual
    *  arguments, the environment in which the lambda expression was evaluated
    *  will be extended by binding the variables in the formal argument list to
    *  fresh locations, and the corresponding actual argument values will be
    *  stored in those locations. (A fresh location is one that is distinct
    *  from every previously existing location.) Next, the expressions in the
    *  body of the lambda expression (which, if it contains definitions,
    *  represents a letrec* form - see section 4.2.2) will be evaluated
    *  sequentially in the extended environment. The results of the last
    *  expression in the body will be returned as the results of the procedure
    *  call.
    *
    * ----------------------------------------------------------------------- */
    {
      WRITE_DEBUG(car(expression), faint, " ; is <formals>");

      return cons(make<instruction>(mnemonic::LOAD_CLOSURE),
                  body(current_syntactic_context,
                       current_syntactic_continuation,
                       cdr(expression),
                       cons(car(expression), frames), // Extend lexical environment.
                       list(make<instruction>(mnemonic::RETURN))),
                  continuation);
    }

    static SYNTAX(call_with_current_continuation) /* ---------------------------
    *
    *  (define (call-with-current-continuation procedure)
    *    (call-with-current-continuation procedure))
    *
    * ----------------------------------------------------------------------- */
    {
      WRITE_DEBUG(car(expression), faint, " ; is <procedure>");

      return cons(make<instruction>(mnemonic::LOAD_CONTINUATION),
                  continuation,
                  compile(current_syntactic_context,
                          current_syntactic_continuation,
                          car(expression),
                          frames,
                          cons(make<instruction>(mnemonic::CALL), continuation)));
    }

    static SYNTAX(fork_csc) /* -------------------------------------------------
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

    static SYNTAX(assignment) /* -----------------------------------------------
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
        throw syntax_error(make<string>("set!"), expression);
      }
      else if (auto variable = de_bruijn_index(car(expression), frames); variable.is_bound())
      {
        if (variable.is_variadic)
        {
          WRITE_DEBUG(car(expression), faint, " ; is <variadic bound variable> references ", reset, variable.index);

          return compile(context::none,
                         current_syntactic_continuation,
                         cadr(expression),
                         frames,
                         cons(make<instruction>(mnemonic::STORE_VARIADIC), variable.index,
                              continuation));
        }
        else
        {
          WRITE_DEBUG(car(expression), faint, "; is a <bound variable> references ", reset, variable.index);

          return compile(context::none,
                         current_syntactic_continuation,
                         cadr(expression),
                         frames,
                         cons(make<instruction>(mnemonic::STORE_LOCAL), variable.index,
                              continuation));
        }
      }
      else
      {
        WRITE_DEBUG(car(expression), faint, "; is a <free variable>");

        if (let const location = current_syntactic_continuation.locate(car(expression)); static_cast<bool>(current_syntactic_context bitand context::outermost) and cdr(location).is<identifier>())
        {
          throw syntax_error(make<string>("it would be an error to perform a set! on an unbound variable (R7RS 5.3.1)"), expression);
        }
        else
        {
          return compile(context::none,
                         current_syntactic_continuation,
                         cadr(expression),
                         frames,
                         cons(make<instruction>(mnemonic::STORE_GLOBAL), location,
                              continuation));
        }
      }
    }

    static SYNTAX(lvalue) // XXX DEPRECATED
    {
      if (expression.is<null>())
      {
        WRITE_DEBUG(car(expression), faint, " ; is <identifier> of itself");
        return unit;
      }
      else if (auto variable = de_bruijn_index(car(expression), frames); variable.is_bound())
      {
        if (variable.is_variadic)
        {
          WRITE_DEBUG(car(expression), faint, " ; is <identifier> of local variadic ", reset, variable.index);
          return cons(make<instruction>(mnemonic::LOAD_VARIADIC), variable.index,
                      continuation);
        }
        else
        {
          WRITE_DEBUG(car(expression), faint, " ; is <identifier> of local ", reset, variable.index);
          return cons(make<instruction>(mnemonic::LOAD_LOCAL), variable.index,
                      continuation);
        }
      }
      else
      {
        WRITE_DEBUG(car(expression), faint, " ; is <identifier> of free variable");
        return cons(make<instruction>(mnemonic::LOAD_GLOBAL), current_syntactic_continuation.locate(car(expression)),
                    continuation);
      }
    }

    SYNTAX(construct) // XXX DEPRECATED
    {
      return compile(context::none,
                     current_syntactic_continuation,
                     cadr(expression),
                     frames,
                     compile(context::none,
                             current_syntactic_continuation,
                             car(expression),
                             frames,
                             cons(make<instruction>(mnemonic::CONS), continuation)));
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
