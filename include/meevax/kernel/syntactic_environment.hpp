/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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
#include <meevax/kernel/implementation_dependent.hpp>
#include <meevax/kernel/include.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/transformer.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Environment>
  struct syntactic_environment : public virtual pair // (<bound-variables> . <free-variables>)
  {
    /*
                          A Syntactic Closures Macro Facility

                                    by Chris Hanson

                                    9 November 1991
    */
    struct syntactic_closure : public virtual pair // (<syntactic-environment> <free-names> . <expression>)
                             , public identifier
    {
      auto align_with(let const& bound_variables) const
      {
        assert(length(caar(*this)) <= length(bound_variables));

        return append(make_list(length(bound_variables) -
                                length(caar(*this))),
                      caar(*this));
      }

      auto compile(object const& bound_variables,
                   object const& free_variables,
                   object const& continuation) const
      {
        assert(car(*this).template is<syntactic_environment>());

        return car(*this).template as<syntactic_environment>().compile(cddr(*this),
                                                                       align_with(bound_variables),
                                                                       append(cadr(*this), free_variables),
                                                                       continuation);
      }

      auto identify(object const& bound_variables,
                    object const& free_variables) const
      {
        assert(car(*this).template is<syntactic_environment>());

        return car(*this).template as<syntactic_environment>().identify(cddr(*this),
                                                                        align_with(bound_variables),
                                                                        append(cadr(*this), free_variables));
      }

      friend auto operator ==(syntactic_closure const& x, syntactic_closure const& y) -> bool
      {
        /*
           (free-identifier=? id-1 id-2)                              procedure

           Returns #t if the original occurrences of id-1 and id-2 have the
           same binding, otherwise returns #f. free-identifier=? is used to
           look for a literal identifier in the argument to a transformer, such
           as else in a cond clause. A macro definition for syntax-rules would
           use free-identifier=? to look for literals in the input.
        */
        assert(car(x).template is<syntactic_environment>());
        assert(car(y).template is<syntactic_environment>());

        return cddr(x).template is_also<identifier>() and
               cddr(y).template is_also<identifier>() and
               eqv(x.identify(caar(x), unit),
                   y.identify(caar(y), unit));
      }

      friend auto operator <<(std::ostream & os, syntactic_closure const& datum) -> std::ostream &
      {
        if (cddr(datum).template is_also<identifier>())
        {
          return os << underline(cddr(datum));
        }
        else
        {
          return os << magenta("#,(") << blue("make-syntactic-closure ") << faint("#;", car(datum).get()) << magenta(" '") << cadr(datum) << magenta(" '") << cddr(datum) << magenta(")");
        }
      }
    };

    static auto rename(std::string const& variable)
    {
      auto bind = [](auto&& name, auto&& compiler)
      {
        return make<absolute>(make_symbol(name), make<syntax>(name, compiler));
      };

      let static const core_syntactic_environment = make<syntactic_environment>(
        list(),
        list(bind("begin"                          , syntax::sequence                      ),
             bind("call-with-current-continuation!", syntax::call_with_current_continuation),
             bind("current"                        , syntax::current                       ),
             bind("define"                         , syntax::define                        ),
             bind("define-syntax"                  , syntax::define_syntax                 ),
             bind("if"                             , syntax::conditional                   ),
             bind("install"                        , syntax::install                       ),
             bind("lambda"                         , syntax::lambda                        ),
             bind("let-syntax"                     , syntax::let_syntax                    ),
             bind("letrec"                         , syntax::letrec                        ),
             bind("letrec-syntax"                  , syntax::letrec_syntax                 ),
             bind("quote"                          , syntax::quote                         ),
             bind("quote-syntax"                   , syntax::quote_syntax                  ),
             bind("set!"                           , syntax::set                           )));

      return make<syntactic_closure>(core_syntactic_environment, unit, make_symbol(variable));
    }

    struct syntax
    {
      using compiler = std::function<auto (syntactic_environment &,
                                           object const& /* expression      */,
                                           object const& /* bound_variables */,
                                           object const& /* free_variables  */,
                                           object const& /* continuation    */,
                                           bool          /* tail            */) -> object>;

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

      #define COMPILER(NAME)                                                   \
      auto NAME([[maybe_unused]] syntactic_environment & compile,              \
                                 object const& expression,                     \
                [[maybe_unused]] object const& bound_variables,                \
                [[maybe_unused]] object const& free_variables,                 \
                                 object const& continuation,                   \
                [[maybe_unused]] bool tail = false) -> object

      static COMPILER(reference) /* --------------------------------------------
      *
      *  R7RS 4.1.1. Variable references
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
        if (let const& identity = compile.identify(expression, bound_variables, free_variables); identity.is<relative>())
        {
          return cons(make(instruction::load_relative), identity,
                      continuation);
        }
        else if (identity.is<variadic>())
        {
          return cons(make(instruction::load_variadic), identity,
                      continuation);
        }
        else
        {
          assert(identity.is<absolute>());
          return cons(make(instruction::load_absolute), identity,
                      continuation);
        }
      }

      static COMPILER(quote) /* ------------------------------------------------
      *
      *  R7RS 4.1.2. Literal Expressions
      *
      *  (quote <datum>)                                                 syntax
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
      *  (i.e. the value of a literal expression) using a mutation procedure
      *  like set-car! or string-set!.
      *
      * --------------------------------------------------------------------- */
      {
        return cons(make(instruction::load_constant), car(expression).is<syntactic_closure>() ? cddar(expression) : car(expression),
                    continuation);
      }

      static COMPILER(quote_syntax)
      {
        return cons(make(instruction::load_constant), car(expression),
                    continuation);
      }

      static COMPILER(call) /* -------------------------------------------------
      *
      *  R7RS 4.1.3. Procedure calls
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
        return operand(compile,
                       cdr(expression),
                       bound_variables,
                       free_variables,
                       compile(car(expression),
                               bound_variables,
                               free_variables,
                               tail ? list(make(instruction::tail_call))
                                    : cons(make(instruction::call), continuation)));
      }

      static COMPILER(operand)
      {
        if (expression.is<pair>())
        {
          return operand(compile,
                         cdr(expression),
                         bound_variables,
                         free_variables,
                         compile(car(expression),
                                 bound_variables,
                                 free_variables,
                                 cons(make(instruction::cons),
                                      continuation)));
        }
        else
        {
          return compile(expression, bound_variables, free_variables, continuation);
        }
      }

      static COMPILER(lambda) /* -----------------------------------------------
      *
      *  R7RS 4.1.4. Procedures
      *
      *  (lambda <formals> <body>)                                       syntax
      *
      *  Syntax: <Formals> is a formal arguments list as described below, and
      *  <body> is a sequence of zero or more definitions followed by one or
      *  more expressions.
      *
      *  Semantics: A lambda expression evaluates to a procedure. The
      *  environment in effect when the lambda expression was evaluated is
      *  remembered as part of the procedure. When the procedure is later
      *  called with some actual arguments, the environment in which the lambda
      *  expression was evaluated will be extended by binding the variables in
      *  the formal argument list to fresh locations, and the corresponding
      *  actual argument values will be stored in those locations. (A fresh
      *  location is one that is distinct from every previously existing
      *  location.) Next, the expressions in the body of the lambda expression
      *  (which, if it contains definitions, represents a letrec* form - see
      *  section 4.2.2) will be evaluated sequentially in the extended
      *  environment. The results of the last expression in the body will be
      *  returned as the results of the procedure call.
      *
      *  <Formals> have one of the following forms:
      *
      *  (<variable 1> ...): The procedure takes a fixed number of arguments;
      *  when the procedure is called, the arguments will be stored in fresh
      *  locations that are bound to the corresponding variables.
      *
      *  <variable>: The procedure takes any number of arguments; when the
      *  procedure is called, the sequence of actual arguments is converted
      *  into a newly allocated list, and the list is stored in a fresh
      *  location that is bound to <variable>.
      *
      *  (<variable 1> ... <variable n> . <variable n+1>): If a space-delimited
      *  period precedes the last variable, then the procedure takes n or more
      *  arguments, where n is the number of formal arguments before the period
      *  (it is an error if there is not at least one). The value stored in the
      *  binding of the last variable will be a newly allocated list of the
      *  actual arguments left over after all the other actual arguments have
      *  been matched up against the other formal arguments.
      *
      *  It is an error for a <variable> to appear more than once in <formals>.
      *
      *  Each procedure created as the result of evaluating a lambda expression
      *  is (conceptually) tagged with a storage location, in order to make
      *  eqv? and eq? work on procedures (see section 6.1).
      *
      * --------------------------------------------------------------------- */
      {
        return cons(make(instruction::load_closure),
                    body(compile,
                         cdr(expression),
                         cons(car(expression), bound_variables), // Extend scope.
                         free_variables,
                         list(make(instruction::return_))),
                    continuation);
      }

      static auto sweep(syntactic_environment const& compile, // This function must not call compile.
                        object const& binding_specs,
                        object const& form,
                        object const& bound_variables,
                        object const& free_variables) -> pair
      {
        if (not form.is<pair>() or not car(form).is<pair>())
        {
          return pair(reverse(binding_specs), form); // Finish.
        }
        else if (let const& identity = compile.identify(caar(form), bound_variables, free_variables);
                 identity.is<absolute>() and
                 cdr(identity).is<transformer>())
        {
          return sweep(compile,
                       binding_specs,
                       cons(Environment().apply(cadr(identity),
                                                car(form),
                                                make<syntactic_environment>(bound_variables, compile.free_variables()),
                                                cddr(identity)),
                            cdr(form)),
                       bound_variables,
                       free_variables);
        }
        else if (identity.is<absolute>() and
                 cdr(identity).is<syntax>() and
                 cdr(identity).as<syntax>().name == "define") // <form> = ((define ...) <definition or expression>*)
        {
          if (let const& definition = car(form); cadr(definition).is<pair>()) // <form> = ((define (<variable> . <formals>) <body>) <definition or expression>*)
          {
            return sweep(compile,
                         cons(list(caadr(definition), // <variable>
                                   cons(rename("lambda"),
                                        cdadr(definition), // <formals>
                                        cddr(definition))), // <body>
                              binding_specs),
                         cdr(form),
                         bound_variables,
                         free_variables);
          }
          else // <form> = ((define <variable> <expression>) <definition or expression>*)
          {
            return sweep(compile,
                         cons(cdr(definition), binding_specs),
                         cdr(form),
                         bound_variables,
                         free_variables);
          }
        }
        else if (identity.is<absolute>() and
                 cdr(identity).is<syntax>() and
                 cdr(identity).as<syntax>().name == "begin")
        {
          return sweep(compile,
                       binding_specs,
                       append(cdar(form), cdr(form)),
                       bound_variables,
                       free_variables);
        }
        else
        {
          return pair(reverse(binding_specs), form); // Finish.
        }
      }

      static COMPILER(body) /* -------------------------------------------------
      *
      *  5.3.2. Internal definitions
      *
      *  Definitions can occur at the beginning of a <body> (that is, the body
      *  of a lambda, let, let*, letrec, letrec*, let-values, let*-values,
      *  let-syntax, letrec-syntax, parameterize, guard, or case-lambda). Note
      *  that such a body might not be apparent until after expansion of other
      *  syntax. Such definitions are known as internal definitions as opposed
      *  to the global definitions described above. The variables defined by
      *  internal definitions are local to the <body>. That is, <variable> is
      *  bound rather than assigned, and the region of the binding is the
      *  entire <body>. For example,
      *
      *      (let ((x 5))
      *        (define foo (lambda (y) (bar x y)))
      *        (define bar (lambda (a b) (+ (* a b) a)))
      *        (foo (+ x 3)))                                      => 45
      *
      *  An expanded <body> containing internal definitions can always be
      *  converted into a completely equivalent letrec* expression. For example,
      *  the let expression in the above example is equivalent to
      *
      *      (let ((x 5))
      *        (letrec* ((foo (lambda (y) (bar x y)))
      *                  (bar (lambda (a b) (+ (* a b) a))))
      *          (foo (+ x 3))))
      *
      *  Just as for the equivalent letrec* expression, it is an error if it is
      *  not possible to evaluate each <expression> of every internal
      *  definition in a <body> without assigning or referring to the value of
      *  the corresponding <variable> or the <variable> of any of the
      *  definitions that follow it in <body>.
      *
      *  It is an error to define the same identifier more than once in the
      *  same <body>.
      *
      *  Wherever an internal definition can occur, (begin <definition 1> ...)
      *  is equivalent to the sequence of definitions that form the body of the
      *  begin.
      *
      * --------------------------------------------------------------------- */
      {
        if (auto const& [binding_specs, sequence] = sweep(compile, unit, expression, bound_variables, free_variables); binding_specs)
        {
          /*
             (letrec* <binding specs> <sequence>)

                 => ((lambda <variables> <assignments> <sequence>)
                     <dummy 1> ... <dummy n>)

             where <binding specs> = ((<variable 1> <initial 1>) ...
                                      (<variable n> <initial n>))
          */
          auto assignment = [](let const& binding_spec)
          {
            return cons(rename("set!"), binding_spec);
          };

          return compile(cons(cons(rename("lambda"),
                                   map(car, binding_specs), // formals
                                   append(map(assignment, binding_specs),
                                          sequence)),
                              make_list(length(binding_specs), unit)),
                         bound_variables,
                         free_variables,
                         continuation,
                         true);
        }
        else
        {
          return compile(car(sequence),
                         bound_variables,
                         free_variables,
                         cdr(sequence).template is<null>() ? continuation
                                                           : cons(make(instruction::drop),
                                                                  body(compile,
                                                                       cdr(sequence),
                                                                       bound_variables,
                                                                       free_variables,
                                                                       continuation)),
                         cdr(sequence).template is<null>());
        }
      }

      static COMPILER(conditional) /* ------------------------------------------
      *
      *  R7RS 4.1.5. Conditionals
      *
      *  (if <test> <consequent> <alternate>)                            syntax
      *  (if <test> <consequent>)                                        syntax
      *
      *  Syntax: <Test>, <consequent>, and <alternate> are expressions.
      *
      *  Semantics: An if expression is evaluated as follows: first, <test> is
      *  evaluated. If it yields a true value (see section 6.3), then
      *  <consequent> is evaluated and its values are returned. Otherwise
      *  <alternate> is evaluated and its values are returned. If <test> yields
      *  a false value and no <alternate> is specified, then the result of the
      *  expression is unspecified.
      *
      * --------------------------------------------------------------------- */
      {
        if (tail)
        {
          assert(lexical_cast<std::string>(continuation) == "(return)");

          return compile(car(expression), // <test>
                         bound_variables,
                         free_variables,
                         list(make(instruction::tail_select),
                              compile(cadr(expression),
                                      bound_variables,
                                      free_variables,
                                      continuation,
                                      tail),
                              cddr(expression) ? compile(caddr(expression),
                                                         bound_variables,
                                                         free_variables,
                                                         continuation,
                                                         tail)
                                               : list(make(instruction::load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                      make(instruction::return_))));
        }
        else
        {
          return compile(car(expression), // <test>
                         bound_variables,
                         free_variables,
                         cons(make(instruction::select),
                              compile(cadr(expression),
                                      bound_variables,
                                      free_variables,
                                      list(make(instruction::join))),
                              cddr(expression) ? compile(caddr(expression),
                                                         bound_variables,
                                                         free_variables,
                                                         list(make(instruction::join)))
                                               : list(make(instruction::load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                      make(instruction::join)),
                              continuation));
        }
      }

      static COMPILER(set) /* --------------------------------------------------
      *
      *  R7RS 4.1.6. Assignments
      *
      *  (set! <variable> <expression>)                                  syntax
      *
      *  Semantics: <Expression> is evaluated, and the resulting value is
      *  stored in the location to which <variable> is bound. It is an error if
      *  <variable> is not bound either in some region enclosing the set!
      *  expression or else globally. The result of the set! expression is
      *  unspecified.
      *
      * --------------------------------------------------------------------- */
      {
        if (let const& identity = compile.identify(car(expression), bound_variables, free_variables); identity.is<relative>())
        {
          return compile(cadr(expression),
                         bound_variables,
                         free_variables,
                         cons(make(instruction::store_relative), identity,
                              continuation));
        }
        else if (identity.is<variadic>())
        {
          return compile(cadr(expression),
                         bound_variables,
                         free_variables,
                         cons(make(instruction::store_variadic), identity,
                              continuation));
        }
        else
        {
          assert(identity.is<absolute>());

          return compile(cadr(expression),
                         bound_variables,
                         free_variables,
                         cons(make(instruction::store_absolute), identity,
                              continuation));
        }
      }

      static COMPILER(include) /* ----------------------------------------------
      *
      *  R7RS  4.1.7. Inclusion
      *
      *  (include <string 1> <string 2> ...)                             syntax
      *  (include-ci <string 1> <string 2> ...)                          syntax
      *
      *  Semantics: Both include and include-ci take one or more filenames
      *  expressed as string literals, apply an implementation-specific
      *  algorithm to find corresponding files, read the contents of the files
      *  in the specified order as if by repeated applications of read, and
      *  effectively re- place the include or include-ci expression with a
      *  begin expression containing what was read from the files. The
      *  difference between the two is that include-ci reads each file as if it
      *  began with the #!fold-case directive, while include does not.
      *
      *  Note: Implementations are encouraged to search for files in the
      *  directory which contains the including file, and to provide a way for
      *  users to specify other directories to search.
      *
      * --------------------------------------------------------------------- */
      {
        return sequence(compile,
                        meevax::include(expression),
                        bound_variables,
                        free_variables,
                        continuation,
                        tail);
      }

      static COMPILER(include_case_insensitive)
      {
        return sequence(compile,
                        meevax::include(expression, false),
                        bound_variables,
                        free_variables,
                        continuation,
                        tail);
      }

      static COMPILER(implementation_dependent) /* -----------------------------
      *
      *  R7RS 4.2.1. Conditionals
      *
      *  (cond-expand <ce-clause 1> <ce-clause 2> ...)                   syntax
      *
      *  Syntax: The cond-expand expression type provides a way to statically
      *  expand different expressions depending on the implementation. A
      *  <ce-clause> takes the following form: (<feature requirement>
      *  <expression> ...)
      *
      *  The last clause can be an "else clause," which has the form (else
      *  <expression> ...)
      *
      *  A <feature requirement> takes one of the following forms:
      *
      *  - <feature identifier>
      *  - (library <library name>)
      *  - (and <feature requirement> ...)
      *  - (or <feature requirement> ...)
      *  - (not <feature requirement>)
      *
      *  Semantics: Each implementation maintains a list of feature identifiers
      *  which are present, as well as a list of libraries which can be
      *  imported. The value of a <feature requirement> is determined by
      *  replacing each <feature identifier> and (library <library name>) on
      *  the implementation's lists with #t, and all other feature identifiers
      *  and library names with #f, then evaluating the resulting expression as
      *  a Scheme boolean expression under the normal interpretation of and,
      *  or, and not.
      *
      *  A cond-expand is then expanded by evaluating the <feature
      *  requirement>s of successive <ce-clause>s in order until one of them
      *  returns #t. When a true clause is found, the corresponding
      *  <expression>s are expanded to a begin, and the remaining clauses are
      *  ignored. If none of the <feature requirement>s evaluate to #t, then if
      *  there is an else clause, its <expression>s are included. Otherwise,
      *  the behavior of the cond-expand is unspecified. Unlike cond,
      *  cond-expand does not depend on the value of any variables.
      *
      *  The exact features provided are implementation-defined, but for
      *  portability a core set of features is given in appendix B.
      *
      * --------------------------------------------------------------------- */
      {
        return sequence(compile,
                        meevax::implementation_dependent(expression),
                        bound_variables,
                        free_variables,
                        continuation,
                        tail);
      }

      static COMPILER(letrec) /* -----------------------------------------------
      *
      *  R7RS 4.2.2. Binding constructs
      *
      *  (letrec <bindings> <body>)                                      syntax
      *
      *  Syntax: <Bindings> has the form
      *
      *      ((<variable 1> <init 1>) ...),
      *
      *  and <body> is a sequence of zero or more definitions followed by one
      *  or more expressions as described in section 4.1.4. It is an error for
      *  a <variable> to appear more than once in the list of variables being
      *  bound.
      *
      *  Semantics: The <variable>s are bound to fresh locations holding
      *  unspecified values, the <init>s are evaluated in the resulting
      *  environment (in some unspecified order), each <variable> is assigned
      *  to the result of the corresponding <init>, the <body> is evaluated in
      *  the resulting environment, and the values of the last expression in
      *  <body> are returned. Each binding of a <variable> has the entire
      *  letrec expression as its region, making it possible to define mutually
      *  recursive procedures.
      *
      *  One restriction on letrec is very important: if it is not possible to
      *  evaluate each <init> without assigning or referring to the value of
      *  any <variable>, it is an error. The restriction is necessary because
      *  letrec is defined in terms of a procedure call where a lambda
      *  expression binds the <variable>s to the values of the <init>s. In the
      *  most common uses of letrec, all the <init>s are lambda expressions and
      *  the restriction is satisfied automatically.
      *
      * --------------------------------------------------------------------- */
      {
        assert(not tail or lexical_cast<std::string>(continuation) == "(return)");

        let const formals = map(car, car(expression));

        return cons(make(instruction::dummy),
                    operand(compile,
                            map(cadr, car(expression)),
                            cons(formals, bound_variables),
                            free_variables,
                            lambda(compile,
                                   cons(formals, cdr(expression)), // (<formals> <body>)
                                   bound_variables,
                                   free_variables,
                                   tail ? list(make(instruction::tail_letrec))
                                        : cons(make(instruction::letrec), continuation))));
      }

      // NOTE: Binding constructs other than letrec are implemented as macros.

      static COMPILER(sequence) /* ---------------------------------------------
      *
      *  R7RS 4.2.3. Sequencing
      *
      *  Both of Scheme's sequencing constructs are named begin, but the two
      *  have slightly different forms and uses:
      *
      *  (begin <expression or definition> ...)                          syntax
      *
      *  This form of begin can appear as part of a <body>, or at the outermost
      *  level of a <program>, or at the REPL, or directly nested in a begin
      *  that is itself of this form. It causes the contained expressions and
      *  definitions to be evaluated exactly as if the enclosing begin
      *  construct were not present.
      *
      *  Rationale: This form is commonly used in the output of macros (see
      *  section 4.3) which need to generate multiple definitions and splice
      *  them into the context in which they are expanded.
      *
      *  (begin <expression 1> <expression 2> ...)                       syntax
      *
      *  This form of begin can be used as an ordinary expression. The
      *  <expression>s are evaluated sequentially from left to right, and the
      *  values of the last <expression> are returned. This expression type is
      *  used to sequence side effects such as assignments or input and output.
      *
      *  Note that there is a third form of begin used as a library
      *  declaration: see section 5.6.1.
      *
      * --------------------------------------------------------------------- */
      {
        /*
           The top-level sequential expression may contain macro definitions.
           In that case, the macro definition must be compiled before the macro
           is used (the evaluation order of function arguments in C++ is not
           specified, but in most environments they are evaluated from right to
           left). Therefore, the first expression is compiled separately and
           then combined with the compiled result of the remaining expressions
           by append.
        */

        if (cdr(expression).is<null>()) // is tail sequence
        {
          return compile(car(expression),
                         bound_variables,
                         free_variables,
                         continuation,
                         tail);
        }
        else if (let const head = compile(car(expression), // Head expression or definition
                                          bound_variables,
                                          free_variables,
                                          unit);
                 head.is<null>()) // The syntax define-syntax creates a transformer from transformer-spec at compile time and registers it in the global environment. The syntax define-syntax is effectively a compile-time side-effect of the syntax environment and does nothing at run-time.
        {
          return sequence(compile,
                          cdr(expression), // rest expressions
                          bound_variables,
                          free_variables,
                          continuation,
                          tail);
        }
        else
        {
          return append(head,
                        cons(make(instruction::drop), // Pop result of head expression
                             sequence(compile,
                                      cdr(expression), // Rest expression or definitions
                                      bound_variables,
                                      free_variables,
                                      continuation,
                                      tail)));
        }
      }

      // NOTE: R7RS 4.2.4. Iteration is implemented as macros.

      // NOTE: R7RS 4.2.5. Delayed evaluation is implemented as macros.

      // NOTE: R7RS 4.2.6. Dynamic bindings are implemented as macros.

      // NOTE: R7RS 4.2.7. Exception handling is implemented as macros.

      // NOTE: R7RS 4.2.8. Quasiquotation is implemented as macros.

      // NOTE: R7RS 4.2.9. Case-lambda is implemented as macros.

      static COMPILER(let_syntax) /* -------------------------------------------
      *
      *  R7RS 4.3.1. Binding constructs for syntactic keywords
      *
      *  (let-syntax <bindings> <body>)                                  syntax
      *
      *  Syntax: <Bindings> has the form
      *
      *      ((<keyword> <transformer spec>) ...)
      *
      *  Each <keyword> is an identifier, each <transformer spec> is an
      *  instance of syntax-rules, and <body> is a sequence of one or more
      *  definitions followed by one or more expressions. It is an error for a
      *  <keyword> to appear more than once in the list of keywords being
      *  bound.
      *
      *  Semantics: The <body> is expanded in the syntactic environment
      *  obtained by extending the syntactic environment of the let-syntax
      *  expression with macros whose keywords are the <keyword>s, bound to the
      *  specified transformers. Each binding of a <keyword> has <body> as its
      *  region.
      *
      * --------------------------------------------------------------------- */
      {
        let const environment = make<syntactic_environment>(bound_variables, compile.free_variables());

        auto formal = [&](let const& syntax_spec)
        {
          return make<absolute>(car(syntax_spec), // <keyword>
                                make<transformer>(Environment().execute(compile(cadr(syntax_spec), // <transformer spec>
                                                                                bound_variables)),
                                                  environment));
        };

        return compile(cons(cons(rename("lambda"),
                                 map(formal, car(expression)), // <formals>
                                 cdr(expression)), // <body>
                            unit), // dummy
                       bound_variables,
                       free_variables,
                       continuation);
      }

      static COMPILER(letrec_syntax) /* ----------------------------------------
      *
      *  R7RS 4.3.1. Binding constructs for syntactic keywords
      *
      *  (letrec-syntax <bingings> <body>)                               syntax
      *
      *  Syntax: Same as for let-syntax.
      *
      *  Semantics: The <body> is expanded in the syntactic environment
      *  obtained by extending the syntactic environment of the letrec-syntax
      *  expression with macros whose keywords are the <keywords>s, bound to
      *  the specified transformers. Each binding of a <keywords> has the
      *  <transformer spec>s as well as the <body> within its region, so the
      *  transformers can transcribe expressions into uses of the macros
      *  introduced by the letrec-syntax expression.
      *
      * --------------------------------------------------------------------- */
      {
        let const environment = make<syntactic_environment>(bound_variables, compile.free_variables());

        auto formal = [&](let const& syntax_spec)
        {
          return make<absolute>(car(syntax_spec), // <keyword>
                                make<transformer>(Environment().execute(compile(cadr(syntax_spec), // <transformer spec>
                                                                                bound_variables)),
                                                  environment));
        };

        let const formals = map(formal, car(expression));

        car(environment) = cons(formals, bound_variables);

        return compile(cons(cons(rename("lambda"),
                                 formals,
                                 cdr(expression)), // <body>
                            unit), // dummy
                       bound_variables,
                       free_variables,
                       continuation);
      }

      // NOTE: R7RS 4.3.2. Pattern language is implemented as macros.

      // NOTE: R7RS 4.3.3. Signaling errors in macro transformers is implemented as macros.

      static COMPILER(define) /* -----------------------------------------------
      *
      *  5.3.1. Top level definitions
      *
      *  At the outermost level of a program, a definition
      *
      *      (define <variable> <expression>)
      *
      *  has essentially the same effect as the assignment expression
      *
      *      (set! <variable> <expression>)
      *
      *  if <variable> is bound to a non-syntax value. However, if <variable>
      *  is not bound, or is a syntactic keyword, then the definition will bind
      *  <variable> to a new location before performing the assignment, whereas
      *  it would be an error to perform a set! on an unbound variable.
      *
      * --------------------------------------------------------------------- */
      {
        if (bound_variables.is<null>())
        {
          if (car(expression).is<pair>()) // (define (<variable> . <formals>) <body>)
          {
            return compile(cons(rename("lambda"), cdar(expression), cdr(expression)),
                           bound_variables,
                           free_variables,
                           cons(make(instruction::store_absolute), compile.identify(caar(expression), bound_variables, free_variables),
                                continuation));
          }
          else // (define <variable> <expression>)
          {
            return compile(cdr(expression) ? cadr(expression) : unspecified,
                           bound_variables,
                           free_variables,
                           cons(make(instruction::store_absolute), compile.identify(car(expression), bound_variables, free_variables),
                                continuation));
          }
        }
        else
        {
          throw error(make<string>("definition cannot appear in this syntactic-context"));
        }
      }

      static COMPILER(define_syntax) /* ----------------------------------------
      *
      *  R7RS 5.4. Syntax definitions
      *
      *  Syntax definitions have this form:
      *
      *  (define-syntax <keyword> <transformer spec>)
      *
      *  <Keyword> is an identifier, and the <transformer spec> is an instance
      *  of syntax-rules. Like variable definitions, syntax definitions can
      *  appear at the outermost level or nested within a body.
      *
      *  If the define-syntax occurs at the outermost level, then the global
      *  syntactic environment is extended by binding the <keyword> to the
      *  specified transformer, but previous expansions of any global binding
      *  for <keyword> remain unchanged. Otherwise, it is an internal syntax
      *  definition, and is local to the <body> in which it is defined. Any use
      *  of a syntax keyword before its corresponding definition is an error.
      *  In particular, a use that precedes an inner definition will not apply
      *  an outer definition.
      *
      *  Macros can expand into definitions in any context that permits them.
      *  However, it is an error for a definition to define an identifier whose
      *  binding has to be known in order to determine the meaning of the
      *  definition itself, or of any preceding definition that belongs to the
      *  same group of internal definitions. Similarly, it is an error for an
      *  internal definition to define an identifier whose binding has to be
      *  known in order to determine the boundary between the internal
      *  definitions and the expressions of the body it belongs to. For
      *  example, the following are errors:
      *
      * --------------------------------------------------------------------- */
      {
        let const identity = compile.identify(car(expression), unit, unit);

        cdr(identity) = make<transformer>(Environment().execute(compile(cadr(expression),
                                                                        bound_variables)),
                                          make<syntactic_environment>(bound_variables,
                                                                      compile.free_variables()));

        return cons(make(instruction::load_constant), unspecified,
                    continuation);
      }

      // NOTE: R7RS 5.5. Record-type definitions is implemented as macros.

      static COMPILER(call_with_current_continuation) /* -----------------------
      *
      *  (define (call-with-current-continuation procedure)
      *    (call-with-current-continuation! procedure))
      *
      * --------------------------------------------------------------------- */
      {
        assert(expression.is<pair>());
        assert(cdr(expression).is<null>());

        return cons(make(instruction::load_continuation),
                    continuation,
                    compile(car(expression),
                            bound_variables,
                            free_variables,
                            list(make(instruction::tail_call)), // The first argument passed to call-with-current-continuation must be called via a tail call.
                            tail));
      }

      static COMPILER(current) /* ----------------------------------------------
      *
      *  (current <register name>)                                       syntax
      *
      * --------------------------------------------------------------------- */
      {
        return cons(make(instruction::current), car(expression),
                    continuation);
      }

      static COMPILER(install) /* ----------------------------------------------
      *
      *  (install <register name> <expression>)                          syntax
      *
      * --------------------------------------------------------------------- */
      {
        return compile(cadr(expression),
                       bound_variables,
                       free_variables,
                       cons(make(instruction::install), car(expression),
                            continuation));
      }

      #undef COMPILER
    };

    auto operator ()(object const& expression,
                     object const& bound_variables = unit, // list of <formals>
                     object const& free_variables = unit,
                     object const& continuation = list(make(instruction::stop)),
                     bool tail = false) -> object
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
        return cons(make(instruction::load_constant), unit, continuation);
      }
      else if (not expression.is<pair>()) /* -----------------------------------
      *
      *  R7RS 4.1.1. Variable references
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
        if (expression.is<symbol>())
        {
          return syntax::reference(*this, expression, bound_variables, free_variables, continuation, tail);
        }
        else if (expression.is<syntactic_closure>())
        {
          if (let const& identity = std::as_const(*this).identify(expression, bound_variables, free_variables); is_truthy(identity)) // The syntactic-closure is an alias
          {
            return syntax::reference(*this, expression, bound_variables, free_variables, continuation, tail);
          }
          else // The syntactic-closure is a syntactic-keyword.
          {
            return expression.as<syntactic_closure>().compile(bound_variables, free_variables, continuation);
          }
        }
        else // is <self-evaluating>
        {
          return cons(make(instruction::load_constant), expression, continuation);
        }
      }
      else if (let const& identity = std::as_const(*this).identify(car(expression), bound_variables, free_variables);
               identity.is<absolute>() and cdr(identity).is<transformer>())
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
        assert(cadr(identity).is<closure>());
        assert(cddr(identity).is<syntactic_environment>());

        return compile(Environment().apply(cadr(identity),
                                           expression,
                                           make<syntactic_environment>(bound_variables, this->free_variables()),
                                           cddr(identity)),
                       bound_variables,
                       free_variables,
                       continuation,
                       tail);
      }
      else if (identity.is<absolute>() and cdr(identity).is<syntax>())
      {
        return cdr(identity).as<syntax>().compile(*this, cdr(expression), bound_variables, free_variables, continuation, tail);
      }
      else
      {
        return syntax::call(*this, expression, bound_variables, free_variables, continuation, tail);
      }
    }

    using pair::pair;

    template <typename... Ts>
    auto compile(Ts&&... xs) -> decltype(auto)
    {
      return operator ()(std::forward<decltype(xs)>(xs)...);
    }

    auto define(object const& variable, object const& value = undefined) -> void
    {
      assert(identify(variable, unit, unit).template is<absolute>());
      cdr(identify(variable, unit, unit)) = value;
    }

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> void
    {
      if constexpr (std::is_constructible_v<T, std::string const&, Ts...>)
      {
        return define(make_symbol(name), make<T>(name, std::forward<decltype(xs)>(xs)...));
      }
      else
      {
        return define(make_symbol(name), make<T>(std::forward<decltype(xs)>(xs)...));
      }
    }

    auto free_variables() const noexcept -> object const&
    {
      return second;
    }

    auto free_variables() noexcept -> object &
    {
      return second;
    }

    auto identify(object const& variable,
                  object const& bound_variables,
                  object const& free_variables) const -> object
    {
      if (not variable.is_also<identifier>())
      {
        return f;
      }
      else
      {
        auto i = identity::index(0);

        for (auto outer = bound_variables; outer.is<pair>(); ++i, outer = cdr(outer))
        {
          auto j = identity::index(0);

          for (auto inner = outer.is<pair>() ? car(outer) : unit; not inner.is<null>(); ++j, inner = inner.is<pair>() ? cdr(inner) : unit)
          {
            if (inner.is<pair>() and car(inner).is<absolute>() and eq(caar(inner), variable))
            {
              return car(inner);
            }
            else if (inner.is<pair>() and eq(car(inner), variable))
            {
              return make<relative>(make(i), make(j));
            }
            else if (inner.is_also<identifier>() and eq(inner, variable))
            {
              return make<variadic>(make(i), make(j));
            }
          }
        }

        if (variable.is<syntactic_closure>())
        {
          return variable.as<syntactic_closure>().identify(bound_variables, free_variables);
        }
        else
        {
          return assq(variable, this->free_variables());
        }
      }
    }

    auto identify(object const& variable,
                  object const& bound_variables,
                  object const& free_variables)
    {
      if (not variable.is_also<identifier>())
      {
        return f;
      }
      else if (let const& identity = std::as_const(*this).identify(variable, bound_variables, free_variables); is_truthy(identity))
      {
        return identity;
      }
      else
      {
        /*
           At the outermost level of a program, a definition

               (define <variable> <expression>)

           has essentially the same effect as the assignment expression

               (set! <variable> <expression>)

           if <variable> is bound to a non-syntax value. However, if <variable>
           is not bound, or is a syntactic keyword, then the definition will
           bind <variable> to a new location before performing the assignment,
           whereas it would be an error to perform a set! on an unbound
           variable.
        */
        return car(this->free_variables() = cons(make<absolute>(variable, undefined), this->free_variables()));
      }
    }

    auto bound_variables() const noexcept -> object const&
    {
      return first;
    }

    auto bound_variables() noexcept -> object &
    {
      return first;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP
