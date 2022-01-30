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
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/identifier.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/option.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>
#include <meevax/kernel/syntactic_procedure.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename environment>
  class machine // TR-SECD machine.
  {
    friend environment;

    machine()
    {}

    IMPORT(environment, global, const);

  protected:
    let s, // stack (holding intermediate results and return address)
        e, // environment (giving values to symbols)
        c, // code (instructions yet to be executed)
        d; // dump (s e c . d)

    struct transformer : public environment
    {
      using environment::s;
      using environment::e;
      using environment::c;
      using environment::d;

      syntactic_continuation const sk;

      explicit transformer() /* ------------------------------------------------
      *
      *  Since the base class environment inherits from pair, all arguments
      *  given to make<transformer> are forwarded directly to the virtual base
      *  class pair. After that, the constructor of the base class environment
      *  is called to set up the environment. This constructor is called after
      *  them.
      *
      * --------------------------------------------------------------------- */
        : sk { spec().template as<continuation>().c().template as<syntactic_continuation>() }
      {
        auto const& k = spec().template as<continuation>();

        s = k.s();
        e = k.e();
        c = compile(context::outermost, *this, sk.expression(), sk.frames());
        d = k.d();

        spec() = environment::execute();

        environment::reset();
      }

      auto macroexpand(const_reference keyword, const_reference form) /* -------
      *
      *  <Transformer-spec> is implemented as a closure. Since closure::c is
      *  terminated by the return instruction, it is necessary to put a stop
      *  instruction in the dump register (this stop instruction is preset by
      *  the constructor of the transformer).
      *
      *  transformer::macroexpand is never called recursively. This is because
      *  in the normal macro expansion performed by machine::compile, control
      *  is returned to machine::compile each time the macro is expanded one
      *  step. As an exception, there are cases where this transformer is given
      *  to the eval procedure as an <environment-spacifier>, but there is no
      *  problem because the stack control at that time is performed by
      *  environment::evaluate.
      *
      * --------------------------------------------------------------------- */
      {
        d = cons(s, e, c, d);
        c =                            spec().template as<closure>().c();
        e = cons(keyword, cdr(form)) | spec().template as<closure>().e();
        s = unit;

        return environment::execute();
      }

      auto spec() -> reference
      {
        return environment::first;
      }

      auto spec() const -> const_reference
      {
        return environment::first;
      }

      friend auto operator <<(std::ostream & os, transformer const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << green("fork/csc ") << datum.sk.expression() << magenta(")");
      }
    };

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
      context const current_context,
      environment & current_environment,
      const_reference expression,
      const_reference frames = unit,
      const_reference current_continuation = list(make<instruction>(mnemonic::stop))) -> object
    {
      if (expression.is<null>()) /* --------------------------------------------
      *
      *  (<operator> <operand 1> ...)                                    syntax
      *
      *  Note: In many dialects of Lisp, the empty list, (), is a legitimate
      *  expression evaluating to itself. In Scheme, it is an error.
      *
      * --------------------------------------------------------------------- */
      {
        return cons(make<instruction>(mnemonic::load_constant), unit, current_continuation);
      }
      else if (not expression.is<pair>()) /* -----------------------------------
      *
      *  <variable>                                                      syntax
      *
      *  An expression consisting of a variable (section 3.1) is a variable
      *  reference. The value of the variable reference is the value stored in
      *  the location to which the variable is bound. It is an error to
      *  reference an unbound variable.
      *
      * --------------------------------------------------------------------- */
      {
        if (expression.is<symbol>() or expression.is_also<identifier>())
        {
          if (let const& identifier = current_environment.rename(expression, frames); identifier.is<absolute>())
          {
            return cons(make<instruction>(mnemonic::load_absolute), identifier,
                        current_continuation);
          }
          else
          {
            return cons(identifier.is<relative>() ? make<instruction>(mnemonic::load_relative)
                                                  : make<instruction>(mnemonic::load_variadic), cdr(identifier),
                        current_continuation);
          }
        }
        else // is <self-evaluating>
        {
          return cons(make<instruction>(mnemonic::load_constant), expression,
                      current_continuation);
        }
      }
      else if (let const& identifier = std::as_const(current_environment).rename(car(expression), frames); identifier.is<keyword>())
      {
        let & binding = identifier.as<keyword>().binding();

        if (not binding.is<transformer>()) // DIRTY HACK
        {
          auto env = environment();

          env.global() = current_environment.global();

          env.s = current_environment.s;
          env.e = current_environment.e;
          env.c = binding;
          env.d = current_environment.d;

          binding = env.execute();
        }

        return compile(context::none,
                       current_environment,
                       binding.as<transformer>().macroexpand(binding, expression),
                       frames,
                       current_continuation);
      }
      else if (let const& applicant = identifier.is<absolute>() ? identifier.as<absolute>().binding() : car(expression); applicant.is_also<syntax>())
      {
        return applicant.as<syntax>().transform(current_context,
                                                current_environment,
                                                cdr(expression),
                                                frames,
                                                current_continuation);
      }
      else if (applicant.is<transformer>())
      {
        return compile(context::none,
                       current_environment,
                       applicant.as<transformer>().macroexpand(applicant, expression),
                       frames,
                       current_continuation);
      }
      else /* ------------------------------------------------------------------
      *
      *  (<operator> <operand 1> ...)                                    syntax
      *
      *  A procedure call is written by enclosing in parentheses an expression
      *  for the procedure to be called followed by expressions for the
      *  arguments to be passed to it. The operator and operand expressions are
      *  evaluated (in an unspecified order) and the resulting procedure is
      *  passed the resulting arguments.
      *
      *  The procedures in this document are available as the values of
      *  variables exported by the standard libraries. For example, the
      *  addition and multiplication procedures in the above examples are the
      *  values of the variables + and * in the base library. New procedures
      *  are created by evaluating lambda expressions (see section 4.1.4).
      *
      *  Procedure calls can return any number of values (see values in section
      *  6.10). Most of the procedures defined in this report return one value
      *  or, for procedures such as apply, pass on the values returned by a
      *  call to one of their arguments. Exceptions are noted in the individual
      *  descriptions.
      *
      *  Note: In contrast to other dialects of Lisp, the order of evaluation
      *  is unspecified, and the operator expression and the operand
      *  expressions are always evaluated with the same evaluation rules.
      *
      *  Note: Although the order of evaluation is otherwise unspecified, the
      *  effect of any concurrent evaluation of the operator and operand
      *  expressions is constrained to be consistent with some sequential order
      *  of evaluation. The order of evaluation may be chosen differently for
      *  each procedure call.
      *
      *  Note: In many dialects of Lisp, the empty list, (), is a legitimate
      *  expression evaluating to itself. In Scheme, it is an error.
      *
      * ------------------------------------------------------------------ */
      {
        return operand(context::none,
                       current_environment,
                       cdr(expression),
                       frames,
                       compile(context::none,
                               current_environment,
                               car(expression),
                               frames,
                               cons(make<instruction>(current_context & context::tail ? mnemonic::tail_call : mnemonic::call),
                                    current_continuation)));
      }
    }

    template <auto Option = option::none>
    inline auto execute() -> object
    {
    decode:
      if constexpr (Option & option::trace)
      {
        std::cerr << faint("; s = ") << s << "\n"
                  << faint("; e = ") << e << "\n"
                  << faint("; c = ") << c << "\n"
                  << faint("; d = ") << d << "\n" << std::endl;
      }

      switch (car(c).template as<instruction>().value)
      {
      case mnemonic::load_relative: /* -----------------------------------------
        *
        *  s  e (%load-relative (i . j) . c) d => (x . s) e c d
        *
        *  where x = (list-ref (list-ref E i) j)
        *
        *    i = (caadr c)
        *    j = (cdadr c)
        *
        * ------------------------------------------------------------------- */
        s = cons(list_ref(list_ref(e, caadr(c)), cdadr(c)), s);
        c = cddr(c);
        goto decode;

      case mnemonic::load_variadic: /* -----------------------------------------
        *
        *  s  e (%load-variadic (i . j) . c) d => (x . s) e c d
        *
        *  where x = (list-tail (list-ref E i) j)
        *
        * ------------------------------------------------------------------- */
        s = cons(list_tail(list_ref(e, caadr(c)), cdadr(c)), s);
        c = cddr(c);
        goto decode;

      case mnemonic::load_constant: /* -----------------------------------------
        *
        *  s e (%load-constant <object> . c) d => (x . s) e c d
        *
        * ------------------------------------------------------------------- */
        s = cons(cadr(c), s);
        c = cddr(c);
        goto decode;

      case mnemonic::load_absolute: /* -----------------------------------------
        *
        *  s e (%load-absolute <identifier> . c) d => (x . s) e c d
        *
        *  where <identifier> = (<symbol> . x)
        *
        * ------------------------------------------------------------------- */
        s = cons(cdadr(c), s);
        c = cddr(c);
        goto decode;

      case mnemonic::load_closure: /* ------------------------------------------
        *
        *  s e (%load-closure c' . c) d => (<closure> . s) e c d
        *
        *  where <closure> = (c' . e)
        *
        * ------------------------------------------------------------------- */
        s = cons(make<closure>(cadr(c), e), s);
        c = cddr(c);
        goto decode;

      case mnemonic::load_continuation: /* -------------------------------------
        *
        *  s e (%load-continuation c1 . c2) d => ((<continuation>) . s) e c2 d
        *
        *  where continuation = (s e c1 . d)
        *
        * ------------------------------------------------------------------- */
        s = cons(list(make<continuation>(s, e, cadr(c), d)), s);
        c = cddr(c);
        goto decode;

      case mnemonic::fork: /* --------------------------------------------------
        *
        *  s e (%fork c1 . c2) d => (<transformer> . s) e c2 d
        *
        * ------------------------------------------------------------------- */
        s = cons(make<transformer>(make<continuation>(s, e, cadr(c), d), global()), s);
        c = cddr(c);
        goto decode;

      case mnemonic::select: /* ------------------------------------------------
        *
        *  (<boolean> . s) e (%select c1 c2 . c) d => s e c' (c . d)
        *
        *  where c' = (if <boolean> c1 c2)
        *
        * ------------------------------------------------------------------- */
        d = cons(cdddr(c), d);
        [[fallthrough]];

      case mnemonic::tail_select: /* -------------------------------------------
        *
        *  (<boolean> . s) e (%select c1 c2 . c) d => s e c' d
        *
        *  where c' = (if <boolean> c1 c2)
        *
        * ------------------------------------------------------------------- */
        c = if_(car(s)) ? cadr(c) : caddr(c);
        s = cdr(s);
        goto decode;

      case mnemonic::join: /* --------------------------------------------------
        *
        *  s e (%join) (c . d) => s e c d
        *
        * ------------------------------------------------------------------- */
        assert(cdr(c).is<null>());
        c = car(d);
        d = cdr(d);
        goto decode;

      case mnemonic::define: /* ------------------------------------------------
        *
        *  (x' . s) e (%define <identifier> . c) d => (x' . s) e c d
        *
        *  where <identifier> = (<symbol> . x := x')
        *
        * ------------------------------------------------------------------- */
        cdadr(c) = car(s);
        c = cddr(c);
        goto decode;

      case mnemonic::letrec_syntax:
        [[fallthrough]];

      case mnemonic::let_syntax: /* --------------------------------------------
        *
        *  s e (%let_syntax <syntactic-continuation> . c) d => s e c' d
        *
        * ------------------------------------------------------------------- */
        std::swap(c.as<pair>(),
                  body(context::none,
                       static_cast<environment &>(*this),
                       cadr(c).template as<syntactic_continuation>().expression(),
                       cadr(c).template as<syntactic_continuation>().frames(),
                       cddr(c)
                      ).template as<pair>());
        goto decode;

      case mnemonic::call:
        if (let const& callee = car(s); callee.is<closure>()) /* ---------------
        *
        *  (<closure> xs . s) e (%call . c) d => () (xs . e') c' (s e c . d)
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
        else if (callee.is_also<procedure>()) /* -------------------------------
        *
        *  (<procedure> xs . s) e (%call . c) d => (x . s) e c d
        *
        *  where x = procedure(xs)
        *
        * ------------------------------------------------------------------- */
        {
          s = callee.as<procedure>().apply(cadr(s)) | cddr(s);
          c = cdr(c);
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *  (<continuation> xs . s) e (%call . c) d => (xs . s') e' c' d'
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

      case mnemonic::tail_call:
        if (let const& callee = car(s); callee.is<closure>()) /* ---------------
        *
        *  (<closure> xs . s) e (%tail-call . c) d => () (xs . e') c' d
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        {
          c = car(callee);
          e = cons(cadr(s), cdr(callee));
          s = unit;
        }
        else if (callee.is_also<procedure>()) /* -------------------------------
        *
        *  (<procedure> xs . s) e (%call . c) d => (x . s) e c d
        *
        *  where x = procedure(xs)
        *
        * ------------------------------------------------------------------- */
        {
          s = callee.as<procedure>().apply(cadr(s)) | cddr(s);
          c = cdr(c);
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *  (<continuation> xs . s)  e (%call . c) d => (xs . s') e' c' d'
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

      case mnemonic::dummy: /* -------------------------------------------------
        *
        *  s e (%dummy . c) d => s (<null> . e) c d
        *
        * ------------------------------------------------------------------- */
        e = cons(unit, e);
        c = cdr(c);
        goto decode;

      case mnemonic::letrec: /* ------------------------------------------------
        *
        *  (<closure> xs . s) (<unit> . e) (%letrec . c) d => () (set-car! e' xs) c' (s e c . d)
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

      case mnemonic::return_: /* -----------------------------------------------
        *
        *  (x . s)  e (%return . c) (s' e' c' . d) => (x . s') e' c' d
        *
        * ------------------------------------------------------------------- */
        s = cons(car(s), pop(d));
        e = pop(d);
        c = pop(d);
        goto decode;

      case mnemonic::cons: /* --------------------------------------------------
        *
        *  (x y . s) e (%cons . c) d => ((x . y) . s) e c d
        *
        * ------------------------------------------------------------------- */
        s = cons(cons(car(s), cadr(s)), cddr(s));
        c = cdr(c);
        goto decode;

      case mnemonic::drop: /* --------------------------------------------------
        *
        *  (x . s) e (%drop . c) d => s e c d
        *
        * ------------------------------------------------------------------- */
        s = cdr(s);
        c = cdr(c);
        goto decode;

      case mnemonic::store_absolute: /* ----------------------------------------
        *
        *  (x . s) e (%store-absolute <identifier> . c) d => (x' . s) e c d
        *
        *  where <identifier> = (<symbol> . x')
        *
        * ------------------------------------------------------------------- */
        if (let const& binding = cadr(c); cdr(binding).is<null>())
        {
          cdr(binding) = car(s);
        }
        else
        {
          cdr(binding) = car(s);
        }
        c = cddr(c);
        goto decode;

      case mnemonic::store_relative: /* ----------------------------------------
        *
        *  (x . s) e (%store-relative (i . j) . c) d => (x' . s) e c d
        *
        * ------------------------------------------------------------------- */
        car(list_tail(list_ref(e, caadr(c)), cdadr(c))) = car(s);
        c = cddr(c);
        goto decode;

      case mnemonic::store_variadic: /* ----------------------------------------
        *
        *  (x . s) e (%store-variadic (i . j) . c) d => (x' . s) e c d
        *
        * ------------------------------------------------------------------- */
        cdr(list_tail(list_ref(e, caadr(c)), cdadr(c))) = car(s);
        c = cddr(c);
        goto decode;

      default: // ERROR
      case mnemonic::stop: /* --------------------------------------------------
        *
        *  (x . s) e (%stop . c) d => s e (%stop . c) d
        *
        * ------------------------------------------------------------------- */
        return pop(s); // return car(s);
      }
    }

    inline auto reset() -> void
    {
      s = unit;
      e = unit;
      c = list(make<instruction>(mnemonic::stop));
      d = unit;
    }

  protected:
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
      else if (let const& identifier = current_environment.rename(car(expression), frames); identifier.is<absolute>())
      {
        return compile(context::none,
                       current_environment,
                       cadr(expression),
                       frames,
                       cons(make<instruction>(mnemonic::store_absolute), identifier,
                            current_continuation));
      }
      else
      {
        return compile(context::none,
                       current_environment,
                       cadr(expression),
                       frames,
                       cons(identifier.is<relative>() ? make<instruction>(mnemonic::store_relative)
                                                      : make<instruction>(mnemonic::store_variadic), cdr(identifier), // De Bruijn index
                            current_continuation));
      }
    }

    static SYNTAX(body)
    {
      auto is_definition = [&](const_reference form)
      {
        if (form.is<pair>())
        {
          if (let const& identifier = std::as_const(current_environment).rename(car(form), frames); identifier.is<absolute>())
          {
            if (let const& callee = cdr(identifier); callee.is<syntax>())
            {
              return callee.as<syntax>().name == "define";
            }
          }
        }

        return false;
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
        return compile(current_context | context::tail,
                       current_environment,
                       car(expression),
                       frames,
                       current_continuation);
      }
      else if (auto const& [binding_specs, body] = sweep(expression); binding_specs)
      {
        /*
           (letrec* <binding specs> <body>)

               => ((lambda <variables> <assignments> <body>) <initials>)

           where <binding specs> = ((<variable 1> <init 1>) ...)
        */
        return compile(current_context,
                       current_environment,
                       cons(cons(make<syntax>("lambda", lambda),
                                 unzip1(binding_specs),
                                 append(map(curry(cons)(make<syntax>("set!", assignment)), binding_specs), body)),
                            make_list(length(binding_specs), undefined_object)),
                       frames,
                       current_continuation);
      }
      else
      {
        return compile(current_context,
                       current_environment,
                       car(expression),
                       frames,
                       cons(make<instruction>(mnemonic::drop),
                            sequence(current_context,
                                     current_environment,
                                     cdr(expression),
                                     frames,
                                     current_continuation)));
      }
    }

    static SYNTAX(call_with_current_continuation) /* ---------------------------
    *
    *  (define (call-with-current-continuation procedure)
    *    (call-with-current-continuation! procedure))
    *
    * ----------------------------------------------------------------------- */
    {
      return cons(make<instruction>(mnemonic::load_continuation),
                  current_continuation,
                  compile(current_context,
                          current_environment,
                          car(expression),
                          frames,
                          cons(make<instruction>(mnemonic::call), current_continuation)));
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
      if (current_context & context::tail)
      {
        auto consequent =
          compile(context::tail,
                  current_environment,
                  cadr(expression),
                  frames,
                  list(make<instruction>(mnemonic::return_)));

        auto alternate =
          cddr(expression)
            ? compile(context::tail,
                      current_environment,
                      caddr(expression),
                      frames,
                      list(make<instruction>(mnemonic::return_)))
            : list(make<instruction>(mnemonic::load_constant), unspecified_object,
                   make<instruction>(mnemonic::return_));

        return compile(context::none,
                       current_environment,
                       car(expression), // <test>
                       frames,
                       cons(make<instruction>(mnemonic::tail_select), consequent, alternate,
                            cdr(current_continuation)));
      }
      else
      {
        auto consequent =
          compile(context::none,
                  current_environment,
                  cadr(expression),
                  frames,
                  list(make<instruction>(mnemonic::join)));

        auto alternate =
          cddr(expression)
            ? compile(context::none,
                      current_environment,
                      caddr(expression),
                      frames,
                      list(make<instruction>(mnemonic::join)))
            : list(make<instruction>(mnemonic::load_constant), unspecified_object,
                   make<instruction>(mnemonic::join));

        return compile(context::none,
                       current_environment,
                       car(expression), // <test>
                       frames,
                       cons(make<instruction>(mnemonic::select), consequent, alternate,
                            current_continuation));
      }
    }

    static SYNTAX(construction)
    {
      return compile(context::none,
                     current_environment,
                     cadr(expression),
                     frames,
                     compile(context::none,
                             current_environment,
                             car(expression),
                             frames,
                             cons(make<instruction>(mnemonic::cons), current_continuation)));
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
      if (frames.is<null>() or (current_context & context::outermost))
      {
        if (car(expression).is<pair>()) // (define (f . <formals>) <body>)
        {
          return compile(context::none,
                         current_environment,
                         cons(make<syntax>("lambda", lambda), cdar(expression), cdr(expression)),
                         frames,
                         cons(make<instruction>(mnemonic::define), current_environment.rename(caar(expression)),
                              current_continuation));
        }
        else // (define x ...)
        {
          return compile(context::none,
                         current_environment,
                         cdr(expression) ? cadr(expression) : unspecified_object,
                         frames,
                         cons(make<instruction>(mnemonic::define), current_environment.rename(car(expression)),
                              current_continuation));
        }
      }
      else
      {
        throw syntax_error(make<string>("definition cannot appear in this syntactic-context"));
      }
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
      return cons(make<instruction>(mnemonic::fork),
                  make<syntactic_continuation>(car(expression), frames),
                  current_continuation);
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
      return cons(make<instruction>(mnemonic::load_closure),
                  body(current_context,
                       current_environment,
                       cdr(expression),
                       cons(car(expression), frames), // Extend lexical environment.
                       list(make<instruction>(mnemonic::return_))),
                  current_continuation);
    }

    static SYNTAX(let_syntax) /* -----------------------------------------------
    *
    *  (let-syntax <bindings> <body>)                                    syntax
    *
    *  Syntax: <Bindings> has the form
    *
    *      ((<keyword> <transformer spec>) . . . )
    *
    *  Each <keyword> is an identifier, each <transformer spec> is an instance
    *  of syntax-rules, and <body> is a sequence of one or more definitions
    *  followed by one or more expressions. It is an error for a <keyword> to
    *  appear more than once in the list of keywords being bound.
    *
    *  Semantics: The <body> is expanded in the syntactic environment obtained
    *  by extending the syntactic environment of the let-syntax expression with
    *  macros whose keywords are the <keyword>s, bound to the specified
    *  transformers. Each binding of a <keyword> has <body> as its region.
    *
    * ----------------------------------------------------------------------- */
    {
      auto make_keyword = [&](let const& binding)
      {
        return make<keyword>(car(binding),
                             compile(context::outermost,
                                     current_environment,
                                     cadr(binding),
                                     frames));
      };

      auto const [bindings, body]  = unpair(expression);

      return cons(make<instruction>(mnemonic::let_syntax),
                  make<syntactic_continuation>(body, cons(map(make_keyword, bindings)), frames),
                  current_continuation);
    }

    static SYNTAX(letrec_syntax) /* --------------------------------------------
    *
    *  (letrec-syntax <bingings> <body>)                                 syntax
    *
    *  Syntax: Same as for let-syntax.
    *
    *  Semantics: The <body> is expanded in the syntactic environment obtained
    *  by extending the syntactic environment of the letrec-syntax expression
    *  with macros whose keywords are the <keywords>s, bound to the specified
    *  transformers. Each binding of a <keywords> has the <transformer spec>s
    *  as well as the <body> within its region, so the transformers can
    *  transcribe expressions into uses of the macros introduced by the
    *  letrec-syntax expression.
    *
    * ----------------------------------------------------------------------- */
    {
      let keywords = map([](let const& binding)
                         {
                           return make<keyword>(car(binding), cadr(binding));
                         },
                         car(expression));

      for (let const& k : keywords)
      {
        k.as<keyword>().binding() = compile(context::outermost,
                                            current_environment,
                                            k.as<keyword>().binding(),
                                            cons(keywords, frames));
      }

      return cons(make<instruction>(mnemonic::letrec_syntax),
                  make<syntactic_continuation>(cdr(expression), cons(keywords, frames)),
                  current_continuation);
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
      auto const& [variables, inits] = unzip2(car(expression));

      return cons(make<instruction>(mnemonic::dummy),
                  operand(context::none,
                          current_environment,
                          inits,
                          cons(variables, frames),
                          lambda(context::none,
                                 current_environment,
                                 cons(variables, cdr(expression)), // (<formals> <body>)
                                 frames,
                                 cons(make<instruction>(mnemonic::letrec),
                                      current_continuation))));
    }

    static SYNTAX(literal) /* --------------------------------------------------
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
      return cons(make<instruction>(mnemonic::load_constant), car(expression),
                  current_continuation);
    }

    static SYNTAX(operand)
    {
      if (expression.is<pair>())
      {
        return operand(context::none,
                       current_environment,
                       cdr(expression),
                       frames,
                       compile(context::none,
                               current_environment,
                               car(expression),
                               frames,
                               cons(make<instruction>(mnemonic::cons),
                                    current_continuation)));
      }
      else
      {
        return compile(context::none, current_environment, expression, frames, current_continuation);
      }
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
      if (current_context & context::outermost)
      {
        if (cdr(expression).is<null>())
        {
          return compile(current_context,
                         current_environment,
                         car(expression),
                         frames,
                         current_continuation);
        }
        else
        {
          return compile(context::outermost,
                         current_environment,
                         car(expression),
                         frames,
                         cons(make<instruction>(mnemonic::drop),
                              sequence(context::outermost,
                                       current_environment,
                                       cdr(expression),
                                       frames,
                                       current_continuation)));
        }
      }
      else
      {
        if (cdr(expression).is<null>()) // is tail sequence
        {
          return compile(current_context,
                         current_environment,
                         car(expression),
                         frames,
                         current_continuation);
        }
        else
        {
          return compile(context::none,
                         current_environment,
                         car(expression), // head expression
                         frames,
                         cons(make<instruction>(mnemonic::drop), // pop result of head expression
                              sequence(context::none,
                                       current_environment,
                                       cdr(expression), // rest expressions
                                       frames,
                                       current_continuation)));
        }
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
