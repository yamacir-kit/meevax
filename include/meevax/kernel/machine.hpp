/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/instruction_level_procedure.hpp>
#include <meevax/kernel/notation.hpp>
#include <meevax/kernel/option.hpp>
#include <meevax/kernel/stack.hpp>
#include <meevax/kernel/syntactic_continuation.hpp>

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

    IMPORT(environment, global_environment, const);
    IMPORT(environment, syntactic_environment, );

  protected:
    let s, // stack (holding intermediate results and return address)
        e, // environment (giving values to symbols)
        c, // code (instructions yet to be executed)
        d; // dump (s e c . d)

    struct transformer
    {
      let const expression;

      let const mac_env;

      explicit transformer(const_reference expression, const_reference mac_env)
        : expression { expression }
        , mac_env { mac_env }
      {
        assert(expression.is<closure>());
      }

      virtual auto expand(const_reference, const_reference) -> object = 0; /* --
      *
      *  Scheme programs can define and use new derived expression types,
      *  called macros. Program-defined expression types have the syntax
      *
      *      (<keyword> <datum>...)
      *
      *  where <keyword> is an identifier that uniquely determines the
      *  expression type. This identifier is called the syntactic keyword, or
      *  simply keyword, of the macro. The number of the <datum>s, and their
      *  syntax, depends on the expression type.
      *
      *  Each instance of a macro is called a use of the macro. The set of
      *  rules that specifies how a use of a macro is transcribed into a more
      *  primitive expression is called the transformer of the macro.
      *
      *  NOTE: <Transformer-spec> is implemented as a closure. Since closure::c
      *  is terminated by the return instruction, it is necessary to put a stop
      *  instruction in the dump register (this stop instruction is preset by
      *  the constructor of the transformer).
      *
      *  NOTE: transformer::expand is never called recursively. This is because
      *  in the normal macro expansion performed by machine::compile, control
      *  is returned to machine::compile each time the macro is expanded one
      *  step. As an exception, there are cases where this transformer is given
      *  to the eval procedure as an <environment-spacifier>, but there is no
      *  problem because the stack control at that time is performed by
      *  environment::evaluate.
      *
      * --------------------------------------------------------------------- */
    };

    struct generic_macro_transformer : public transformer
    {
      using transformer::expression;
      using transformer::mac_env;
      using transformer::transformer;

      auto expand(const_reference form, const_reference use_env) -> object override
      {
        return mac_env.template as<environment>().apply(expression, list(form, use_env, mac_env));
      }

      friend auto operator <<(std::ostream & os, generic_macro_transformer const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << green("generic-macro-transformer ") << faint("#;", &datum) << magenta(")");
      }
    };

    struct syntactic_closure
    {
      let const enclosure;

      let const free_variables;

      let const expression;

      auto notate()
      {
        return enclosure.as<environment>().notate(expression, enclosure.as<environment>().syntactic_environment());
      }

      friend auto operator <<(std::ostream & os, syntactic_closure const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << blue("make-syntactic-closure ") << datum.enclosure << " " << magenta("'") << datum.free_variables << " " << magenta("'") << datum.expression << magenta(")");
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
      const_reference current_expression,
      const_reference current_syntactic_environment = unit,
      const_reference current_continuation = list(make<instruction>(mnemonic::stop))) -> object
    {
      if (current_expression.is<null>()) /* ------------------------------------
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
      else if (not current_expression.is<pair>()) /* -----------------------------------
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
        if (current_expression.is<symbol>() or current_expression.is_also<absolute>())
        {
          let const& n = current_environment.notate(current_expression,
                                                    current_syntactic_environment);

          return cons(n.as<notation>().make_load_instruction(), n,
                      current_continuation);
        }
        else if (current_expression.is<syntactic_closure>())
        {
          if (let const& n = std::as_const(current_environment).notate(current_expression, current_syntactic_environment); select(n))
          {
            return cons(n.as<notation>().make_load_instruction(), n,
                        current_continuation);
          }
          else
          {
            return compile(current_context,
                           current_expression.as<syntactic_closure>().enclosure.template as<environment>(),
                           current_expression.as<syntactic_closure>().expression,
                           current_expression.as<syntactic_closure>().enclosure.template as<environment>().syntactic_environment(),
                           current_continuation);
          }
        }
        else // is <self-evaluating>
        {
          return cons(make<instruction>(mnemonic::load_constant), current_expression,
                      current_continuation);
        }
      }
      else if (let const& notation = std::as_const(current_environment).notate(car(current_expression), current_syntactic_environment); notation.is<keyword>())
      {
        assert(notation.as<keyword>().strip().is_also<transformer>());

        return compile(context::none,
                       current_environment,
                       notation.as<keyword>().strip().as<transformer>().expand(current_expression,
                                                                               current_environment.fork(current_syntactic_environment)),
                       current_syntactic_environment,
                       current_continuation);
      }
      else if (let const& applicant = notation.is<absolute>() ? notation.as<absolute>().strip() : car(current_expression); applicant.is_also<syntax>())
      {
        return applicant.as<syntax>().compile(current_context,
                                              current_environment,
                                              cdr(current_expression),
                                              current_syntactic_environment,
                                              current_continuation);
      }
      else if (applicant.is_also<transformer>())
      {
        return compile(context::none,
                       current_environment,
                       applicant.as<transformer>().expand(current_expression,
                                                          current_environment.fork(current_syntactic_environment)),
                       current_syntactic_environment,
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
                       cdr(current_expression),
                       current_syntactic_environment,
                       compile(context::none,
                               current_environment,
                               car(current_expression),
                               current_syntactic_environment,
                               cons(make<instruction>(current_context & context::tail ? mnemonic::tail_call : mnemonic::call), current_syntactic_environment,
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
      case mnemonic::load_absolute: /* -----------------------------------------
        *
        *  s e (%load-absolute <absolute notation> . c) d => (x . s) e c d
        *
        *  where <absolute notation> = (<symbol> . x)
        *
        * ------------------------------------------------------------------- */
        [[fallthrough]];

      case mnemonic::load_relative: /* -----------------------------------------
        *
        *  s  e (%load-relative <relative notation> . c) d => (x . s) e c d
        *
        *  where <relative notation> = (<symbol> i . j)
        *
        *        x = (list-ref (list-ref e i) j)
        *
        * ------------------------------------------------------------------- */
        [[fallthrough]];

      case mnemonic::load_variadic: /* -----------------------------------------
        *
        *  s  e (%load-variadic <variadic notation> . c) d => (x . s) e c d
        *
        *  where <variadic notation> = (<symbol> i . j)
        *
        *        x = (list-tail (list-ref e i) j)
        *
        * ------------------------------------------------------------------- */
        s = cons(cadr(c).template as<notation>().strip(e), s);
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
        c = select(car(s)) ? cadr(c) : caddr(c);
        s = cdr(s);
        goto decode;

      case mnemonic::join: /* --------------------------------------------------
        *
        *  s e (%join) (c . d) => s e c d
        *
        * ------------------------------------------------------------------- */
        assert(cdr(c).template is<null>());
        c = car(d);
        d = cdr(d);
        goto decode;

      case mnemonic::define: /* ------------------------------------------------
        *
        *  (x' . s) e (%define <notation> . c) d => (x' . s) e c d
        *
        *  where <notation> = (<symbol> . x := x')
        *
        * ------------------------------------------------------------------- */
        cadr(c).template as<absolute>().strip() = car(s);
        c = cddr(c);
        goto decode;

      case mnemonic::define_syntax: /* -----------------------------------------
        *
        *  (<closure> . s) e (%define <notation> . c) d => (x' . s) e c d
        *
        *  where <notation> = (<symbol> . x := <transformer>)
        *
        * ------------------------------------------------------------------- */
        assert(car(s).template is<closure>());
        cadr(c).template as<absolute>().strip() = make<generic_macro_transformer>(car(s), static_cast<environment const&>(*this).fork(unit));
        c = cddr(c);
        goto decode;

      case mnemonic::let_syntax: /* --------------------------------------------
        *
        *  s e (%let-syntax <syntactic-continuation> . c) d => s e c' d
        *
        * ------------------------------------------------------------------- */
        [&]()
        {
          let const& syntactic_environment = cadr(c).template as<syntactic_continuation>().syntactic_environment();

          for (let const& keyword_ : car(syntactic_environment))
          {
            let & binding = keyword_.as<keyword>().strip();

            let const& f = environment(static_cast<environment const&>(*this)).execute(binding);

            binding = make<generic_macro_transformer>(f, static_cast<environment const&>(*this).fork(unit));
          }
        }();

        std::swap(c.as<pair>(),
                  body(context::none,
                       static_cast<environment &>(*this),
                       cadr(c).template as<syntactic_continuation>().expression(),
                       cadr(c).template as<syntactic_continuation>().syntactic_environment(),
                       cddr(c)
                      ).template as<pair>());

        goto decode;

      case mnemonic::letrec_syntax: /* -----------------------------------------
        *
        *  s e (%letrec-syntax <syntactic-continuation> . c) d => s e c' d
        *
        * ------------------------------------------------------------------- */
        [&]() // DIRTY HACK!!!
        {
          auto env = environment(static_cast<environment const&>(*this));

          auto const [transformer_specs, body] = unpair(cadr(c).template as<syntactic_continuation>().expression());

          for (let const& transformer_spec : transformer_specs)
          {
            env.execute(compile(context::outermost,
                                env,
                                cons(make<syntax>("define-syntax", define_syntax), transformer_spec),
                                cadr(c).template as<syntactic_continuation>().syntactic_environment()));
          }

          std::swap(c.as<pair>(),
                    machine::body(context::outermost,
                                  env,
                                  body,
                                  cadr(c).template as<syntactic_continuation>().syntactic_environment(),
                                  cddr(c)
                                 ).template as<pair>());
        }();
        goto decode;

      case mnemonic::call:
        if (let const& callee = car(s); callee.is<closure>()) /* ---------------
        *
        *  (<closure> xs . s) e (%call <syntactic-environment> . c) d => () (xs . e') c' (s e c . d)
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        {
          d = cons(cddr(s), e, cddr(c), d);
          c =               callee.as<closure>().c();
          e = cons(cadr(s), callee.as<closure>().e());
          s = unit;
        }
        else if (callee.is_also<procedure>()) /* -------------------------------
        *
        *  (<procedure> xs . s) e (%call <syntactic-environment> . c) d => (x . s) e c d
        *
        *  where x = procedure(xs)
        *
        * ------------------------------------------------------------------- */
        {
          s = cons(callee.as<procedure>().call(cadr(s), cadr(c), static_cast<environment &>(*this)), cddr(s));
          c = cddr(c);
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *  (<continuation> xs . s) e (%call <syntactic-environment> . c) d => (xs . s') e' c' d'
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
        *  (<closure> xs . s) e (%tail-call <syntactic-environment> . c) d => () (xs . e') c' d
        *
        *  where <closure> = (c' . e')
        *
        * ------------------------------------------------------------------- */
        {
          c =               callee.as<closure>().c();
          e = cons(cadr(s), callee.as<closure>().e());
          s = unit;
        }
        else if (callee.is_also<procedure>()) /* -------------------------------
        *
        *  (<procedure> xs . s) e (%tail-call <syntactic-environment> . c) d => (x . s) e c d
        *
        *  where x = procedure(xs)
        *
        * ------------------------------------------------------------------- */
        {
          s = cons(callee.as<procedure>().call(cadr(s), cadr(c), static_cast<environment &>(*this)), cddr(s));
          c = cddr(c);
        }
        else if (callee.is<continuation>()) /* ---------------------------------
        *
        *  (<continuation> xs . s)  e (%tail-call <syntactic-environment> . c) d => (xs . s') e' c' d'
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
        *  (x' . s) e (%store-absolute <absolute notation> . c) d => (x' . s) e c d
        *
        *  where <absolute notation> = (<symbol> . x:=x')
        *
        * ------------------------------------------------------------------- */
        [[fallthrough]];

      case mnemonic::store_relative: /* ----------------------------------------
        *
        *  (x . s) e (%store-relative <relative notation> . c) d => (x' . s) e c d
        *
        * ------------------------------------------------------------------- */
        [[fallthrough]];

      case mnemonic::store_variadic: /* ----------------------------------------
        *
        *  (x . s) e (%store-variadic <variadic notation> . c) d => (x' . s) e c d
        *
        * ------------------------------------------------------------------- */
        cadr(c).template as<notation>().strip(e) = car(s);
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

    static auto notate(const_reference variable, const_reference syntactic_environment) -> object
    {
      for (auto outer = std::begin(syntactic_environment); outer != std::end(syntactic_environment); ++outer)
      {
        for (auto inner = std::begin(*outer); inner != std::end(*outer); ++inner)
        {
          if (inner.is<pair>() and (*inner).is<keyword>() and eq((*inner).as<keyword>().symbol(), variable))
          {
            return *inner;
          }
          else if (inner.is<pair>() and eq(*inner, variable))
          {
            // NOTE: A class that inherits from pair behaves as if it were `cons*` when given three or more arguments.
            static_assert(std::is_base_of<pair, relative>::value);

            return make<relative>(variable,
                                  make<exact_integer>(std::distance(std::begin(syntactic_environment), outer)),
                                  make<exact_integer>(std::distance(std::begin(*outer), inner)));
          }
          else if (inner.is<symbol>() and eq(inner, variable))
          {
            // NOTE: A class that inherits from pair behaves as if it were `cons*` when given three or more arguments.
            static_assert(std::is_base_of<pair, variadic>::value);

            return make<variadic>(variable,
                                  make<exact_integer>(std::distance(std::begin(syntactic_environment), outer)),
                                  make<exact_integer>(std::distance(std::begin(*outer), inner)));
          }
        }
      }

      return variable.is<syntactic_closure>() ? variable.as<syntactic_closure>().notate() : f;
    }

    inline auto reset() -> void
    {
      s = unit;
      e = unit;
      c = list(make<instruction>(mnemonic::stop));
      d = unit;
    }

  protected:
    static SYNTAX(set) /* ------------------------------------------------------
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
      let const& notation = current_environment.notate(car(current_expression), current_syntactic_environment);

      return compile(context::none,
                     current_environment,
                     cadr(current_expression),
                     current_syntactic_environment,
                     cons(notation.as<meevax::notation>().make_store_instruction(), notation,
                          current_continuation));
    }

    static SYNTAX(body)
    {
      auto is_definition = [&](const_reference form)
      {
        if (form.is<pair>())
        {
          if (let const& notation = std::as_const(current_environment).notate(car(form), current_syntactic_environment); notation.is<absolute>())
          {
            if (let const& callee = notation.as<absolute>().strip(); callee.is<syntax>())
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
      if (cdr(current_expression).is<null>()) // is tail-sequence
      {
        return compile(current_context | context::tail,
                       current_environment,
                       car(current_expression),
                       current_syntactic_environment,
                       current_continuation);
      }
      else if (auto const& [binding_specs, body] = sweep(current_expression); binding_specs)
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
                                 append(map(curry(cons)(make<syntax>("set!", set)), binding_specs), body)),
                            make_list(length(binding_specs), undefined_object)),
                       current_syntactic_environment,
                       current_continuation);
      }
      else
      {
        return compile(current_context,
                       current_environment,
                       car(current_expression),
                       current_syntactic_environment,
                       cons(make<instruction>(mnemonic::drop),
                            begin(current_context,
                                  current_environment,
                                  cdr(current_expression),
                                  current_syntactic_environment,
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
                          car(current_expression),
                          current_syntactic_environment,
                          cons(make<instruction>(mnemonic::call), current_syntactic_environment,
                               current_continuation)));
    }

    static SYNTAX(if_) /* ------------------------------------------------------
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
                  cadr(current_expression),
                  current_syntactic_environment,
                  list(make<instruction>(mnemonic::return_)));

        auto alternate =
          cddr(current_expression)
            ? compile(context::tail,
                      current_environment,
                      caddr(current_expression),
                      current_syntactic_environment,
                      list(make<instruction>(mnemonic::return_)))
            : list(make<instruction>(mnemonic::load_constant), unspecified_object,
                   make<instruction>(mnemonic::return_));

        return compile(context::none,
                       current_environment,
                       car(current_expression), // <test>
                       current_syntactic_environment,
                       cons(make<instruction>(mnemonic::tail_select), consequent, alternate,
                            cdr(current_continuation)));
      }
      else
      {
        auto consequent =
          compile(context::none,
                  current_environment,
                  cadr(current_expression),
                  current_syntactic_environment,
                  list(make<instruction>(mnemonic::join)));

        auto alternate =
          cddr(current_expression)
            ? compile(context::none,
                      current_environment,
                      caddr(current_expression),
                      current_syntactic_environment,
                      list(make<instruction>(mnemonic::join)))
            : list(make<instruction>(mnemonic::load_constant), unspecified_object,
                   make<instruction>(mnemonic::join));

        return compile(context::none,
                       current_environment,
                       car(current_expression), // <test>
                       current_syntactic_environment,
                       cons(make<instruction>(mnemonic::select), consequent, alternate,
                            current_continuation));
      }
    }

    static SYNTAX(cons_)
    {
      return compile(context::none,
                     current_environment,
                     cadr(current_expression),
                     current_syntactic_environment,
                     compile(context::none,
                             current_environment,
                             car(current_expression),
                             current_syntactic_environment,
                             cons(make<instruction>(mnemonic::cons), current_continuation)));
    }

    static SYNTAX(define) /* ---------------------------------------------------
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
      if (current_syntactic_environment.is<null>() or (current_context & context::outermost))
      {
        if (car(current_expression).is<pair>()) // (define (f . <formals>) <body>)
        {
          return compile(context::none,
                         current_environment,
                         cons(make<syntax>("lambda", lambda), cdar(current_expression), cdr(current_expression)),
                         current_syntactic_environment,
                         cons(make<instruction>(mnemonic::define), current_environment.notate(caar(current_expression), current_syntactic_environment),
                              current_continuation));
        }
        else // (define x ...)
        {
          return compile(context::none,
                         current_environment,
                         cdr(current_expression) ? cadr(current_expression) : unspecified_object,
                         current_syntactic_environment,
                         cons(make<instruction>(mnemonic::define), current_environment.notate(car(current_expression), current_syntactic_environment),
                              current_continuation));
        }
      }
      else
      {
        throw syntax_error(make<string>("definition cannot appear in this syntactic-context"));
      }
    }

    static SYNTAX(define_syntax) /* --------------------------------------------
    *
    *  Syntax definitions have this form:
    *
    *      (define-syntax <keyword> <transformer spec>)
    *
    *  <Keyword> is an identifier, and the <transformer spec> is an instance of
    *  syntax-rules. Like variable definitions, syntax definitions can appear
    *  at the outermost level or nested within a body.
    *
    *  If the define-syntax occurs at the outermost level, then the global
    *  syntactic environment is extended by binding the <keyword> to the
    *  specified transformer, but previous expansions of any global binding for
    *  <keyword> remain unchanged. Otherwise, it is an internal syntax
    *  definition, and is local to the <body> in which it is defined. Any use
    *  of a syntax keyword before its corresponding definition is an error. In
    *  particular, a use that precedes an inner definition will not apply an
    *  outer definition.
    *
    *      (let ((x 1) (y 2))
    *        (define-syntax swap!
    *          (syntax-rules ()
    *            ((swap! a b)
    *             (let ((tmp a))
    *               (set! a b)
    *               (set! b tmp)))))
    *        (swap! x y)
    *        (list x y)) => (2 1)
    *
    *  Macros can expand into definitions in any context that permits them.
    *  However, it is an error for a definition to define an identifier whose
    *  binding has to be known in order to determine the meaning of the
    *  definition itself, or of any preceding definition that belongs to the
    *  same group of internal definitions. Similarly, it is an error for an
    *  internal definition to define an identifier whose binding has to be
    *  known in order to determine the boundary between the internal
    *  definitions and the expressions of the body it belongs to. For example,
    *  the following are errors:
    *
    *      (define define 3)
    *
    *      (begin (define begin list))
    *
    *      (let-syntax ((foo (syntax-rules ()
    *                          ((foo (proc args ...) body ...)
    *                           (define proc
    *                             (lambda (args ...) body ...))))))
    *        (let ((x 3))
    *          (foo (plus x y) (+ x y))
    *          (define foo x)
    *          (plus foo x)))
    *
    * ----------------------------------------------------------------------- */
    {
      return compile(context::none,
                     current_environment,
                     cdr(current_expression) ? cadr(current_expression) : unspecified_object,
                     current_syntactic_environment,
                     cons(make<instruction>(mnemonic::define_syntax), current_environment.notate(car(current_expression), current_syntactic_environment),
                          current_continuation));
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
                       cdr(current_expression),
                       cons(car(current_expression), current_syntactic_environment), // Extend lexical environment.
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
                                     current_syntactic_environment));
      };

      auto const [bindings, body]  = unpair(current_expression);

      return cons(make<instruction>(mnemonic::let_syntax),
                  make<syntactic_continuation>(body,
                                               cons(map(make_keyword, bindings),
                                                    current_syntactic_environment)),
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
      return cons(make<instruction>(mnemonic::letrec_syntax),
                  make<syntactic_continuation>(current_expression, current_syntactic_environment),
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
      auto const& [variables, inits] = unzip2(car(current_expression));

      return cons(make<instruction>(mnemonic::dummy),
                  operand(context::none,
                          current_environment,
                          inits,
                          cons(variables, current_syntactic_environment),
                          lambda(context::none,
                                 current_environment,
                                 cons(variables, cdr(current_expression)), // (<formals> <body>)
                                 current_syntactic_environment,
                                 cons(make<instruction>(mnemonic::letrec),
                                      current_continuation))));
    }

    static SYNTAX(quote) /* ----------------------------------------------------
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
      if (car(current_expression).is<syntactic_closure>())
      {
        return cons(make<instruction>(mnemonic::load_constant), car(current_expression).as<syntactic_closure>().expression,
                    current_continuation);
      }
      else
      {
        return cons(make<instruction>(mnemonic::load_constant), car(current_expression),
                    current_continuation);
      }
    }

    static SYNTAX(quote_syntax)
    {
      return cons(make<instruction>(mnemonic::load_constant), car(current_expression),
                  current_continuation);
    }

    static SYNTAX(operand)
    {
      if (current_expression.is<pair>())
      {
        return operand(context::none,
                       current_environment,
                       cdr(current_expression),
                       current_syntactic_environment,
                       compile(context::none,
                               current_environment,
                               car(current_expression),
                               current_syntactic_environment,
                               cons(make<instruction>(mnemonic::cons),
                                    current_continuation)));
      }
      else
      {
        return compile(context::none,
                       current_environment,
                       current_expression,
                       current_syntactic_environment,
                       current_continuation);
      }
    }

    static SYNTAX(begin) /* ----------------------------------------------------
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
        if (cdr(current_expression).is<null>())
        {
          return compile(current_context,
                         current_environment,
                         car(current_expression),
                         current_syntactic_environment,
                         current_continuation);
        }
        else
        {
          return compile(context::outermost,
                         current_environment,
                         car(current_expression),
                         current_syntactic_environment,
                         cons(make<instruction>(mnemonic::drop),
                              begin(context::outermost,
                                    current_environment,
                                    cdr(current_expression),
                                    current_syntactic_environment,
                                    current_continuation)));
        }
      }
      else
      {
        if (cdr(current_expression).is<null>()) // is tail sequence
        {
          return compile(current_context,
                         current_environment,
                         car(current_expression),
                         current_syntactic_environment,
                         current_continuation);
        }
        else
        {
          return compile(context::none,
                         current_environment,
                         car(current_expression), // head expression
                         current_syntactic_environment,
                         cons(make<instruction>(mnemonic::drop), // pop result of head expression
                              begin(context::none,
                                    current_environment,
                                    cdr(current_expression), // rest expressions
                                    current_syntactic_environment,
                                    current_continuation)));
        }
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_MACHINE_HPP
