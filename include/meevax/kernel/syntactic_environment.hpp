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

#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP

#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/transformer.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Environment>
  struct syntactic_environment : public virtual pair // (<local> . <global>)
  {
    using pair::pair;

  protected:
    struct syntactic_closure : public identifier
    {
      let const syntactic_environment;

      let const free_variables; // Currently ignored

      let const expression;

      explicit syntactic_closure(let const& syntactic_environment,
                                 let const& free_variables,
                                 let const& expression)
        : syntactic_environment { syntactic_environment }
        , free_variables { free_variables }
        , expression { expression }
      {}

      auto compile(object const& current_continuation) const
      {
        return syntactic_environment.as<Environment>().compile(syntactic_environment.as<Environment>(),
                                                               expression,
                                                               syntactic_environment.as<Environment>().scope(),
                                                               current_continuation);
      }

      auto identify() const -> object
      {
        return syntactic_environment.as<Environment>().identify(expression,
                                                                syntactic_environment.as<Environment>().scope());
      }

      friend auto operator ==(syntactic_closure const& x, syntactic_closure const& y) -> bool
      {
        /*
           (free-identifier=? id-1 id-2)                               procedure

           Returns #t if the original occurrences of id-1 and id-2 have the same
           binding, otherwise returns #f. free-identifier=? is used to look for
           a literal identifier in the argument to a transformer, such as else
           in a cond clause. A macro definition for syntax-rules would use
           free-identifier=? to look for literals in the input.
        */
        return x.expression.template is_also<identifier>() and
               y.expression.template is_also<identifier>() and
               eqv(x.syntactic_environment.template as<Environment>().identify(x.expression,
                                                                               x.syntactic_environment.template as<Environment>().scope()),
                   y.syntactic_environment.template as<Environment>().identify(y.expression,
                                                                               y.syntactic_environment.template as<Environment>().scope()));
      }

      friend auto operator <<(std::ostream & os, syntactic_closure const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << blue("make-syntactic-closure ") << datum.syntactic_environment << magenta(" '") << datum.free_variables << magenta("' ") << datum.expression << magenta(")");
      }
    };

  public:
    struct syntax
    {
      using compiler = std::function<auto (Environment &,
                                           object const&,
                                           object const&,
                                           object const&,
                                           object const&) -> object>;

      std::string const name;

      compiler const compile;

      explicit syntax(std::string const& name, compiler const& compile)
        : name { name }
        , compile { compile }
      {}

      friend auto operator <<(std::ostream & os, syntax const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << green("syntax ") << datum.name << magenta(")");
      }
    };

    static auto compile(Environment & current_environment,
                        object const& current_expression,
                        object const& current_scope = unit,
                        object const& current_continuation = list(make(instruction::stop)),
                        object const& current_tail = unspecified) -> object
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
        return cons(make(instruction::load_constant), unit, current_continuation);
      }
      else if (not current_expression.is<pair>()) /* ---------------------------
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
        if (current_expression.is<symbol>())
        {
          if (let const& identity = current_environment.identify(current_expression, current_scope); identity.is<relative>())
          {
            return cons(make(instruction::load_relative), identity,
                        current_continuation);
          }
          else if (identity.is<variadic>())
          {
            return cons(make(instruction::load_variadic), identity,
                        current_continuation);
          }
          else
          {
            assert(identity.is<absolute>());
            return cons(make(instruction::load_absolute), identity,
                        current_continuation);
          }
        }
        else if (current_expression.is<syntactic_closure>())
        {
          if (let const& identity = std::as_const(current_environment).identify(current_expression, current_scope); is_truthy(identity)) // The syntactic-closure is a variable
          {
            if (identity.is<relative>())
            {
              return cons(make(instruction::load_relative), identity,
                          current_continuation);
            }
            else if (identity.is<variadic>())
            {
              return cons(make(instruction::load_variadic), identity,
                          current_continuation);
            }
            else
            {
              assert(identity.is<absolute>());
              return cons(make(instruction::load_absolute), identity,
                          current_continuation);
            }
          }
          else // The syntactic-closure is a syntactic-keyword.
          {
            return current_expression.as<syntactic_closure>().compile(current_continuation);
          }
        }
        else // is <self-evaluating>
        {
          return cons(make(instruction::load_constant), current_expression,
                      current_continuation);
        }
      }
      else if (car(current_expression).is<syntax>())
      {
        return car(current_expression).as<syntax>().compile(current_environment,
                                                            cdr(current_expression),
                                                            current_scope,
                                                            current_continuation,
                                                            current_tail);
      }
      else if (let const& identity = std::as_const(current_environment).identify(car(current_expression), current_scope);
               identity.is<absolute>() and
               identity.as<absolute>().load().is<transformer>())
      {
        /*
           Scheme programs can define and use new derived expression types,
           called macros. Program-defined expression types have the syntax

             (<keyword> <datum>...)

           where <keyword> is an identifier that uniquely determines the
           expression type. This identifier is called the syntactic keyword, or
           simply keyword, of the macro. The number of the <datum>s, and their
           syntax, depends on the expression type.

           Each instance of a macro is called a use of the macro. The set of
           rules that specifies how a use of a macro is transcribed into a more
           primitive expression is called the transformer of the macro.
        */
        assert(car(identity.as<absolute>().load()).is<closure>());
        assert(cdr(identity.as<absolute>().load()).is<Environment>());

        return compile(current_environment,
                       Environment().apply(car(identity.as<absolute>().load()), // <closure>
                                           current_expression,
                                           current_environment.fork(current_scope),
                                           cdr(identity.as<absolute>().load())), // <environment>
                       current_scope,
                       current_continuation,
                       current_tail);
      }
      else if (identity.is<absolute>() and
               identity.as<absolute>().load().is<syntax>())
      {
        return identity.as<absolute>().load().as<syntax>().compile(current_environment,
                                                                   cdr(current_expression),
                                                                   current_scope,
                                                                   current_continuation,
                                                                   current_tail);
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
        return operand(current_environment,
                       cdr(current_expression),
                       current_scope,
                       compile(current_environment,
                               car(current_expression),
                               current_scope,
                               current_tail.is<null>() ? list(make(instruction::tail_call))
                                                       : cons(make(instruction::call), current_continuation)));
      }
    }

    auto global() const noexcept -> object const&
    {
      return second;
    }

    auto global() noexcept -> object &
    {
      return second;
    }

    auto identify(object const& variable, object const& scope) const -> object
    {
      if (not variable.is_also<identifier>())
      {
        return f;
      }
      else
      {
        for (auto outer = std::begin(scope); outer != std::end(scope); ++outer)
        {
          for (auto inner = std::begin(*outer); inner != std::end(*outer); ++inner)
          {
            if (inner.get().is<pair>() and (*inner).is<absolute>() and eq((*inner).as<absolute>().symbol(), variable))
            {
              return *inner;
            }
            else if (inner.get().is<pair>() and eq(*inner, variable))
            {
              return make<relative>(make(static_cast<identity::index>(std::distance(std::begin(scope), outer))),
                                    make(static_cast<identity::index>(std::distance(std::begin(*outer), inner))));
            }
            else if (inner.get().is<symbol>() and eq(inner, variable))
            {
              return make<variadic>(make(static_cast<identity::index>(std::distance(std::begin(scope), outer))),
                                    make(static_cast<identity::index>(std::distance(std::begin(*outer), inner))));
            }
          }
        }

        if (variable.is<syntactic_closure>())
        {
          return variable.as<syntactic_closure>().identify();
        }
        else
        {
          return assq(variable, global());
        }
      }
    }

    auto identify(object const& variable, object const& scope)
    {
      if (not variable.is_also<identifier>())
      {
        return f;
      }
      else if (let const& identity = std::as_const(*this).identify(variable, scope); is_truthy(identity))
      {
        return identity;
      }
      else /* --------------------------------------------------------------------
      *
      *  At the outermost level of a program, a definition
      *
      *      (define <variable> <expression>)
      *
      *  has essentially the same effect as the assignment expression
      *
      *      (set! <variable> <expression>)
      *
      *  if <variable> is bound to a non-syntax value. However, if <variable> is
      *  not bound, or is a syntactic keyword, then the definition will bind
      *  <variable> to a new location before performing the assignment, whereas
      *  it would be an error to perform a set! on an unbound variable.
      *
      * ----------------------------------------------------------------------- */
      {
        return car(global() = cons(make<absolute>(variable, undefined), global()));
      }
    }

    auto scope() const noexcept -> object const&
    {
      return first;
    }

    auto scope() noexcept -> object &
    {
      return first;
    }

  public:
    #define SYNTAX(NAME)                                                       \
    auto NAME([[maybe_unused]] Environment & current_environment,              \
                               object const& current_expression,               \
              [[maybe_unused]] object const& current_scope,                    \
              [[maybe_unused]] object const& current_continuation,             \
              [[maybe_unused]] object const& current_tail = unspecified) -> object

    static SYNTAX(body)
    {
      auto is_definition = [&](object const& form)
      {
        if (form.is<pair>())
        {
          if (let const& identity = std::as_const(current_environment).identify(car(form), current_scope); identity.is<absolute>())
          {
            if (let const& callee = identity.as<absolute>().load(); callee.is<syntax>())
            {
              return callee.as<syntax>().name == "define";
            }
          }
        }

        return false;
      };

      auto sweep = [&](object const& form)
      {
        /*
           <binding specs> = ((<variable> (lambda <formals> <body>)) ...)
        */
        let binding_specs = unit;

        for (auto iter = std::begin(form); iter != std::end(form); ++iter)
        {
          if (is_definition(*iter)) // (define ...)
          {
            if (cadr(*iter).template is<pair>()) // (define (<variable> . <formals>) . <body>)
            {
              binding_specs = cons(list(caadr(*iter), // <variable>
                                        cons(make<syntax>("lambda", lambda),
                                             cdadr(*iter), // <formals>
                                             cddr(*iter))), // <body>
                                   binding_specs);
            }
            else // (define <variable> <expression>)
            {
              binding_specs = cons(cdr(*iter), binding_specs);
            }
          }
          else
          {
            return std::make_pair(reverse(binding_specs), iter.get());
          }
        }

        return std::make_pair(reverse(binding_specs), unit);
      };

      /*
         (lambda <formals> <body>)

         where <body> = <definition>* <expression>* <tail expression>
      */
      if (auto const& [binding_specs, sequence] = sweep(current_expression); binding_specs)
      {
        /*
           (letrec* <binding specs> <sequence>)

               => ((lambda <variables> <assignments> <sequence>)
                   <dummy 1> ... <dummy n>)

           where <binding specs> = ((<variable 1> <initial 1>)
                                    ...
                                    (<variable n> <initial n>))
        */
        return compile(current_environment,
                       cons(cons(make<syntax>("lambda", lambda),
                                 unzip1(binding_specs), // formals
                                 append2(map1([](let const& binding_spec)
                                              {
                                                return cons(make<syntax>("set!", set), binding_spec);
                                              },
                                              binding_specs),
                                         sequence)),
                            make_list(length(binding_specs), unit)),
                       current_scope,
                       current_continuation,
                       unit);
      }
      else if (cdr(current_expression).is<null>())
      {
        return compile(current_environment,
                       car(current_expression),
                       current_scope,
                       current_continuation,
                       cdr(current_expression));
      }
      else
      {
        let const head = compile(current_environment,
                                 car(current_expression),
                                 current_scope,
                                 unit,
                                 cdr(current_expression));

        return append2(head,
                       cons(make(instruction::drop),
                            body(current_environment,
                                 cdr(current_expression),
                                 current_scope,
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
      assert(current_expression.is<pair>());
      assert(cdr(current_expression).is<null>());

      return cons(make(instruction::load_continuation),
                  current_continuation,
                  compile(current_environment,
                          car(current_expression),
                          current_scope,
                          list(make(instruction::tail_call)), // The first argument passed to call-with-current-continuation must be called via a tail call.
                          current_tail));
    }

    static SYNTAX(current) /* --------------------------------------------------
    *
    *  (current <register name>)                                         syntax
    *
    * ----------------------------------------------------------------------- */
    {
      return cons(make(instruction::current), car(current_expression),
                  current_continuation);
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
      if (current_scope.is<null>()) // R7RS 5.3.1. Top level definitions
      {
        if (car(current_expression).is<pair>()) // (define (<variable> . <formals>) <body>)
        {
          return compile(current_environment,
                         cons(make<syntax>("lambda", lambda), cdar(current_expression), cdr(current_expression)),
                         current_scope,
                         cons(make(instruction::store_absolute), current_environment.identify(caar(current_expression), current_scope),
                              current_continuation));
        }
        else // (define <variable> <expression>)
        {
          return compile(current_environment,
                         cdr(current_expression) ? cadr(current_expression) : unspecified,
                         current_scope,
                         cons(make(instruction::store_absolute), current_environment.identify(car(current_expression), current_scope),
                              current_continuation));
        }
      }
      else
      {
        throw error(make<string>("definition cannot appear in this syntactic-context"));
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
      return compile(current_environment,
                     cdr(current_expression) ? cadr(current_expression) : undefined,
                     current_scope,
                     cons(make(instruction::define_syntax), current_environment.identify(car(current_expression), current_scope),
                          current_continuation));
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
      if (current_tail.is<null>())
      {
        assert(lexical_cast<std::string>(current_continuation) == "(return)");

        return compile(current_environment,
                       car(current_expression), // <test>
                       current_scope,
                       list(make(instruction::tail_select),
                            compile(current_environment,
                                    cadr(current_expression),
                                    current_scope,
                                    current_continuation,
                                    current_tail),
                            cddr(current_expression) ? compile(current_environment,
                                                               caddr(current_expression),
                                                               current_scope,
                                                               current_continuation,
                                                               current_tail)
                                                     : list(make(instruction::load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                            make(instruction::return_))));
      }
      else
      {
        return compile(current_environment,
                       car(current_expression), // <test>
                       current_scope,
                       cons(make(instruction::select),
                            compile(current_environment,
                                    cadr(current_expression),
                                    current_scope,
                                    list(make(instruction::join))),
                            cddr(current_expression) ? compile(current_environment,
                                                               caddr(current_expression),
                                                               current_scope,
                                                               list(make(instruction::join)))
                                                     : list(make(instruction::load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                            make(instruction::join)),
                            current_continuation));
      }
    }

    static SYNTAX(install) /* --------------------------------------------------
    *
    *  (install <register name> <expression>)                            syntax
    *
    * ----------------------------------------------------------------------- */
    {
      return compile(current_environment,
                     cadr(current_expression),
                     current_scope,
                     cons(make(instruction::install), car(current_expression),
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
      return cons(make(instruction::load_closure),
                  body(current_environment,
                       cdr(current_expression),
                       cons(car(current_expression), current_scope), // Extend lexical scope.
                       list(make(instruction::return_))),
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
        return make<absolute>(car(binding),
                              compile(current_environment,
                                      cadr(binding),
                                      current_scope));
      };

      auto const [bindings, body]  = unpair(current_expression);

      return cons(make(instruction::let_syntax),
                  cons(body,
                       map1(make_keyword, bindings),
                       current_scope),
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
      assert(not current_tail.is<null>() or lexical_cast<std::string>(current_continuation) == "(return)");

      auto const& [variables, inits] = unzip2(car(current_expression));

      return cons(make(instruction::dummy),
                  operand(current_environment,
                          inits,
                          cons(variables, current_scope),
                          lambda(current_environment,
                                 cons(variables, cdr(current_expression)), // (<formals> <body>)
                                 current_scope,
                                 current_tail.is<null>() ? list(make(instruction::tail_letrec))
                                                         : cons(make(instruction::letrec), current_continuation))));
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
      return cons(make(instruction::letrec_syntax),
                  cons(current_expression, current_scope),
                  current_continuation);
    }

    static SYNTAX(operand)
    {
      if (current_expression.is<pair>())
      {
        return operand(current_environment,
                       cdr(current_expression),
                       current_scope,
                       compile(current_environment,
                               car(current_expression),
                               current_scope,
                               cons(make(instruction::cons),
                                    current_continuation)));
      }
      else
      {
        return compile(current_environment,
                       current_expression,
                       current_scope,
                       current_continuation);
      }
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
      return cons(make(instruction::load_constant), car(current_expression).is<syntactic_closure>() ? car(current_expression).as<syntactic_closure>().expression
                                                                                                    : car(current_expression),
                  current_continuation);
    }

    static SYNTAX(quote_syntax)
    {
      return cons(make(instruction::load_constant), car(current_expression),
                  current_continuation);
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
      if (cdr(current_expression).is<null>()) // is tail sequence
      {
        return compile(current_environment,
                       car(current_expression),
                       current_scope,
                       current_continuation,
                       current_tail);
      }
      else
      {
        return compile(current_environment,
                       car(current_expression), // head expression
                       current_scope,
                       cons(make(instruction::drop), // pop result of head expression
                            sequence(current_environment,
                                     cdr(current_expression), // rest expressions
                                     current_scope,
                                     current_continuation,
                                     current_tail)));
      }
    }

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
      if (let const& identity = current_environment.identify(car(current_expression), current_scope); identity.is<relative>())
      {
        return compile(current_environment,
                       cadr(current_expression),
                       current_scope,
                       cons(make(instruction::store_relative), identity,
                            current_continuation));
      }
      else if (identity.is<variadic>())
      {
        return compile(current_environment,
                       cadr(current_expression),
                       current_scope,
                       cons(make(instruction::store_variadic), identity,
                            current_continuation));
      }
      else
      {
        assert(identity.is<absolute>()); // <Keyword> cannot appear.
        return compile(current_environment,
                       cadr(current_expression),
                       current_scope,
                       cons(make(instruction::store_absolute), identity,
                            current_continuation));
      }
    }

    #undef SYNTAX
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP
