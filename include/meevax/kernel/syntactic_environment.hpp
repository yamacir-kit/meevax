/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#include <meevax/kernel/describable.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/implementation_dependent.hpp>
#include <meevax/kernel/include.hpp>
#include <meevax/kernel/transformer.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Environment>
  struct syntactic_environment : public virtual pair // (<bound-variables> . <free-variables>)
  {
    struct syntactic_closure : public virtual pair // (<syntactic-environment> <free-names> . <expression>)
                             , public identifier
    {
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
               eqv(car(x).template as<syntactic_environment>()
                         .identify(cddr(x),
                                   caar(x),
                                   nullptr),
                   car(y).template as<syntactic_environment>()
                         .identify(cddr(y),
                                   caar(y),
                                   nullptr));
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

    struct syntax : public describable
    {
      auto (*compile)(syntactic_environment &,
                      object const& /* expression      */,
                      object const& /* bound_variables */,
                      object const& /* free_variables  */,
                      object const& /* continuation    */,
                      bool          /* tail            */) -> object;

      auto (*expand)(syntactic_environment const&,
                     object const& expression,
                     object const& bound_variables,
                     object const& free_variables) -> object;

      template <typename Compiler, typename Expander>
      explicit syntax(std::string const& name, Compiler const& compile, Expander const& expand)
        : describable { name }
        , compile { compile }
        , expand { expand }
      {}

      friend auto operator <<(std::ostream & os, syntax const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << green("syntax ") << datum.name << magenta(")");
      }

      #define COMPILER(NAME)                                                   \
      auto NAME([[maybe_unused]] syntactic_environment & compiler,             \
                [[maybe_unused]] object const& expression,                     \
                [[maybe_unused]] object const& bound_variables,                \
                [[maybe_unused]] object const& free_variables,                 \
                [[maybe_unused]] object const& continuation,                   \
                [[maybe_unused]] bool tail = false) -> object

      static COMPILER(reference)
      {
        if (let const& identity = compiler.identify(expression, bound_variables, free_variables); identity.is<relative>())
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

      static COMPILER(quote)
      {
        return cons(make(instruction::load_constant), car(expression).is<syntactic_closure>() ? cddar(expression) : car(expression),
                    continuation);
      }

      static COMPILER(quote_syntax)
      {
        return cons(make(instruction::load_constant), car(expression),
                    continuation);
      }

      static COMPILER(call)
      {
        return operand(compiler,
                       cdr(expression),
                       bound_variables,
                       free_variables,
                       compiler.compile(car(expression),
                                        bound_variables,
                                        free_variables,
                                        tail ? list(make(instruction::tail_call))
                                             : cons(make(instruction::call), continuation)));
      }

      static COMPILER(operand)
      {
        if (expression.is<pair>())
        {
          return operand(compiler,
                         cdr(expression),
                         bound_variables,
                         free_variables,
                         compiler.compile(car(expression),
                                          bound_variables,
                                          free_variables,
                                          cons(make(instruction::cons),
                                               continuation)));
        }
        else
        {
          return compiler.compile(expression, bound_variables, free_variables, continuation);
        }
      }

      static COMPILER(lambda)
      {
        return cons(make(instruction::load_closure),
                    body(compiler,
                         cdr(expression),
                         cons(car(expression), bound_variables), // Extend scope.
                         free_variables,
                         list(make(instruction::return_))),
                    continuation);
      }

      static COMPILER(body)
      {
        if (cdr(expression).template is<null>())
        {
          return compiler.compile(car(expression),
                                  bound_variables,
                                  free_variables,
                                  continuation,
                                  true);
        }
        else
        {
          return compiler.compile(car(expression),
                                  bound_variables,
                                  free_variables,
                                  cons(make(instruction::drop),
                                       body(compiler,
                                            cdr(expression),
                                            bound_variables,
                                            free_variables,
                                            continuation)),
                                  false);
        }
      }

      static COMPILER(conditional)
      {
        if (tail)
        {
          assert(lexical_cast<std::string>(continuation) == "(return)");

          return compiler.compile(car(expression), // <test>
                                  bound_variables,
                                  free_variables,
                                  list(make(instruction::tail_select),
                                       compiler.compile(cadr(expression),
                                                        bound_variables,
                                                        free_variables,
                                                        continuation,
                                                        tail),
                                       cddr(expression) ? compiler.compile(caddr(expression),
                                                                           bound_variables,
                                                                           free_variables,
                                                                           continuation,
                                                                           tail)
                                                        : list(make(instruction::load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                               make(instruction::return_))));
        }
        else
        {
          return compiler.compile(car(expression), // <test>
                                  bound_variables,
                                  free_variables,
                                  cons(make(instruction::select),
                                       compiler.compile(cadr(expression),
                                                        bound_variables,
                                                        free_variables,
                                                        list(make(instruction::join))),
                                       cddr(expression) ? compiler.compile(caddr(expression),
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
        if (let const& identity = compiler.identify(car(expression), bound_variables, free_variables); identity.is<relative>())
        {
          return compiler.compile(cadr(expression),
                                  bound_variables,
                                  free_variables,
                                  cons(make(instruction::store_relative), identity,
                                       continuation));
        }
        else if (identity.is<variadic>())
        {
          return compiler.compile(cadr(expression),
                                  bound_variables,
                                  free_variables,
                                  cons(make(instruction::store_variadic), identity,
                                       continuation));
        }
        else
        {
          assert(identity.is<absolute>());

          return compiler.compile(cadr(expression),
                                  bound_variables,
                                  free_variables,
                                  cons(make(instruction::store_absolute), identity,
                                       continuation));
        }
      }

      static constexpr auto include = nullptr;

      static constexpr auto include_case_insensitive = nullptr;

      static constexpr auto implementation_dependent = nullptr;

      static COMPILER(letrec)
      {
        assert(not tail or lexical_cast<std::string>(continuation) == "(return)");

        let const formals = map(car, car(expression));

        return cons(make(instruction::dummy),
                    operand(compiler,
                            map(cadr, car(expression)),
                            cons(formals, bound_variables),
                            free_variables,
                            lambda(compiler,
                                   cons(formals, cdr(expression)), // (<formals> <body>)
                                   bound_variables,
                                   free_variables,
                                   tail ? list(make(instruction::tail_letrec))
                                        : cons(make(instruction::letrec), continuation))));
      }

      static COMPILER(sequence)
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
          return compiler.compile(car(expression),
                                  bound_variables,
                                  free_variables,
                                  continuation,
                                  tail);
        }
        else if (let const head = compiler.compile(car(expression), // Head expression or definition
                                                   bound_variables,
                                                   free_variables,
                                                   nullptr);
                 head.is<null>()) // The syntax define-syntax creates a transformer from transformer-spec at compile time and registers it in the global environment. The syntax define-syntax is effectively a compile-time side-effect of the syntax environment and does nothing at run-time.
        {
          return sequence(compiler,
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
                             sequence(compiler,
                                      cdr(expression), // Rest expression or definitions
                                      bound_variables,
                                      free_variables,
                                      continuation,
                                      tail)));
        }
      }

      static constexpr auto let_syntax = nullptr;

      static constexpr auto letrec_syntax = nullptr;

      static COMPILER(define)
      {
        assert(bound_variables.is<null>()); // This has been checked on previous passes.

        assert(not car(expression).is<pair>());

        return compiler.compile(cdr(expression) ? cadr(expression) : unspecified,
                                bound_variables,
                                free_variables,
                                cons(make(instruction::store_absolute), compiler.identify(car(expression), bound_variables, free_variables),
                                     continuation));
      }

      static COMPILER(define_syntax)
      {
        let identity = compiler.identify(car(expression), nullptr, nullptr);

        cdr(identity) = make<transformer>(Environment().execute(compiler.compile(cadr(expression),
                                                                                 bound_variables)),
                                          make<syntactic_environment>(bound_variables,
                                                                      compiler.second));

        return cons(make(instruction::load_constant), unspecified,
                    continuation);
      }

      static COMPILER(call_with_current_continuation)
      {
        assert(expression.is<pair>());
        assert(cdr(expression).is<null>());

        return cons(make(instruction::load_continuation),
                    continuation,
                    compiler.compile(car(expression),
                                     bound_variables,
                                     free_variables,
                                     list(make(instruction::tail_call)), // The first argument passed to call-with-current-continuation must be called via a tail call.
                                     tail));
      }

      static COMPILER(current)
      {
        return cons(make(instruction::current), car(expression),
                    continuation);
      }

      static COMPILER(install)
      {
        return compiler.compile(cadr(expression),
                                bound_variables,
                                free_variables,
                                cons(make(instruction::install), car(expression),
                                     continuation));
      }

      #undef COMPILER
    };

    struct expander
    {
      #define EXPANDER(NAME)                                                   \
      auto NAME([[maybe_unused]] syntactic_environment const& expander,        \
                                 object const& expression,                     \
                [[maybe_unused]] object const& bound_variables,                \
                [[maybe_unused]] object const& free_variables) -> object

      static EXPANDER(quote)
      {
        return expression;
      }

      static EXPANDER(quote_syntax)
      {
        return expression;
      }

      static EXPANDER(call)
      {
        return cons(expander.expand(car(expression),
                                    bound_variables,
                                    free_variables),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            free_variables));
      }

      static EXPANDER(operand)
      {
        if (expression.is<pair>())
        {
          return cons(expander.expand(car(expression),
                                      bound_variables,
                                      free_variables),
                      operand(expander,
                              cdr(expression),
                              bound_variables,
                              free_variables));
        }
        else
        {
          return expander.expand(expression, bound_variables, free_variables);
        }
      }

      static EXPANDER(lambda)
      {
        return cons(car(expression) /* lambda */,
                    cons(cadr(expression) /* formals */,
                         body(expander,
                              cddr(expression),
                              cons(cadr(expression) /* formals */, bound_variables),
                              free_variables)));
      }

      static EXPANDER(body)
      {
        if (auto [binding_specs, sequence] = expander.sweep(expression, bound_variables, free_variables); binding_specs)
        {
          /*
             (letrec* <binding specs> <sequence>)

                 => ((lambda <variables> <assignments> <sequence>)
                     <dummy 1> ... <dummy n>)

             where <binding specs> = ((<variable 1> <initial 1>) ...
                                      (<variable n> <initial n>))
          */
          let formals = nullptr;

          let body = sequence;

          for (let const& binding_spec : binding_specs) // The order of the list `binding_specs` returned from the function `sweep` is the reverse of the definition order.
          {
            let const& variable = car(binding_spec);

            formals = cons(variable, formals);

            if (not variable.is<absolute>()) // The binding-spec is not an internal syntax definition.
            {
              body = cons(cons(rename("set!"), binding_spec), body);
            }
          }

          let const current_environment = make<syntactic_environment>(cons(formals, bound_variables),
                                                                      expander.second);

          for (let & formal : formals)
          {
            if (formal.is<absolute>())
            {
              cdr(formal) = make<transformer>(Environment().execute(current_environment.as<syntactic_environment>().compile(cdr(formal) /* <transformer spec> */,
                                                                                                                            car(current_environment))),
                                              current_environment);
            }
          }

          return expander.expand(list(cons(cons(rename("lambda"),
                                                formals,
                                                body),
                                           make_list(length(binding_specs), nullptr))),
                                 bound_variables,
                                 free_variables);
        }
        else if (sequence.template is<pair>())
        {
          return cons(expander.expand(car(sequence),
                                      bound_variables,
                                      free_variables),
                      body(expander,
                           cdr(sequence),
                           bound_variables,
                           free_variables));
        }
        else
        {
          return expander.expand(sequence, bound_variables, free_variables);
        }
      }

      static EXPANDER(conditional)
      {
        return cons(car(expression),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            free_variables));
      }

      static EXPANDER(set)
      {
        return cons(car(expression),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            free_variables));
      }

      static EXPANDER(include)
      {
        return expander.expand(cons(rename("begin"),
                                    meevax::include(cadr(expression))),
                               bound_variables,
                               free_variables);
      }

      static EXPANDER(include_case_insensitive)
      {
        return expander.expand(cons(rename("begin"),
                                    meevax::include(cadr(expression), false)),
                               bound_variables,
                               free_variables);
      }

      static EXPANDER(implementation_dependent)
      {
        return expander.expand(cons(rename("begin"),
                                    meevax::implementation_dependent(cdr(expression))),
                               bound_variables,
                               free_variables);
      }

      static EXPANDER(letrec)
      {
        let const extended_bound_variables = cons(map(car, cadr(expression)), bound_variables);

        return cons(car(expression),
                    map([&](let const& binding)
                        {
                          return list(car(binding),
                                      expander.expand(cadr(binding),
                                                      extended_bound_variables,
                                                      free_variables));
                        },
                        cadr(expression)),
                    body(expander,
                         cddr(expression),
                         extended_bound_variables,
                         free_variables));
      }

      static EXPANDER(sequence)
      {
        if (expression.is<pair>())
        {
          return cons(expander.expand(car(expression),
                                      bound_variables,
                                      free_variables),
                      sequence(expander,
                               cdr(expression),
                               bound_variables,
                               free_variables));
        }
        else
        {
          return expander.expand(expression, bound_variables, free_variables);
        }
      }

      static EXPANDER(let_syntax)
      {
        let const current_environment = make<syntactic_environment>(bound_variables, expander.second);

        auto formal = [&](let const& syntax_spec)
        {
          return make<absolute>(car(syntax_spec) /* keyword */,
                                make<transformer>(Environment().execute(current_environment.as<syntactic_environment>().compile(cadr(syntax_spec), // <transformer spec>
                                                                                                                                bound_variables)),
                                                  current_environment));
        };

        let const formals = map(formal, cadr(expression));

        return expander.expand(list(cons(rename("lambda"),
                                         formals,
                                         cddr(expression) /* body */)),
                               bound_variables,
                               free_variables);
      }

      static EXPANDER(letrec_syntax)
      {
        let current_environment = make<syntactic_environment>(bound_variables, expander.second);

        auto formal = [&](let const& syntax_spec)
        {
          return make<absolute>(car(syntax_spec), // <keyword>
                                make<transformer>(Environment().execute(current_environment.as<syntactic_environment>().compile(cadr(syntax_spec), // <transformer spec>
                                                                                                                                bound_variables)),
                                                  current_environment));
        };

        let const formals = map(formal, cadr(expression));

        car(current_environment) = cons(formals, bound_variables);

        return expander.expand(list(cons(rename("lambda"),
                                         formals,
                                         cddr(expression) /* body */)),
                               bound_variables,
                               free_variables);
      }

      static EXPANDER(define)
      {
        if (bound_variables.is<null>())
        {
          if (cadr(expression).is<pair>()) // (define (<variable> . <formals>) <body>)
          {
            return list(car(expression),
                        caadr(expression) /* variable */,
                        expander.expand(cons(rename("lambda"),
                                             cdadr(expression) /* formals */,
                                             cddr(expression) /* body */),
                                        bound_variables,
                                        free_variables));
          }
          else // (define <variable> <expression>)
          {
            return cons(car(expression),
                        cadr(expression),
                        cddr(expression) ? list(expander.expand(caddr(expression),
                                                                bound_variables,
                                                                free_variables))
                                         : unit);
          }
        }
        else
        {
          throw error(make<string>("definition cannot appear in this syntactic-context"));
        }
      }

      static EXPANDER(define_syntax)
      {
        return list(car(expression),
                    cadr(expression),
                    expander.expand(caddr(expression),
                                    bound_variables,
                                    free_variables));
      }

      static EXPANDER(call_with_current_continuation)
      {
        return cons(car(expression),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            free_variables));
      }

      static EXPANDER(current)
      {
        return cons(car(expression),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            free_variables));
      }

      static EXPANDER(install)
      {
        return cons(car(expression),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            free_variables));
      }

      #undef EXPANDER
    };

    using pair::pair;

    inline auto compile(object const& expression,
                        object const& bound_variables = nullptr, // list of <formals>
                        object const& free_variables = nullptr,
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
        return cons(make(instruction::load_constant), nullptr, continuation);
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
          if (let const& identity = std::as_const(*this).identify(expression, bound_variables, free_variables); identity != f) // The syntactic-closure is an alias
          {
            return syntax::reference(*this, expression, bound_variables, free_variables, continuation, tail);
          }
          else
          {
            assert(car(expression).is<syntactic_environment>());

            return car(expression).as<syntactic_environment>()
                                  .compile(cddr(expression),
                                           unify(caar(expression), bound_variables),
                                           map([&](let const& free_variable)
                                               {
                                                 return cons(free_variable,
                                                             make<syntactic_environment>(bound_variables, free_variables));
                                               },
                                               cadr(expression) /* free-variables of syntactic-closure */,
                                               free_variables),
                                           continuation);
          }
        }
        else // is <self-evaluating>
        {
          return cons(make(instruction::load_constant), expression, continuation);
        }
      }
      else if (let const& identity = std::as_const(*this).identify(car(expression), bound_variables, free_variables); identity.is<absolute>())
      {
        if (cdr(identity).is<transformer>())
        {
          /*
             Scheme programs can define and use new derived expression types,
             called macros. Program-defined expression types have the syntax

               (<keyword> <datum>...)

             where <keyword> is an identifier that uniquely determines the
             expression type. This identifier is called the syntactic keyword,
             or simply keyword, of the macro. The number of the <datum>s, and
             their syntax, depends on the expression type.

             Each instance of a macro is called a use of the macro. The set of
             rules that specifies how a use of a macro is transcribed into a
             more primitive expression is called the transformer of the macro.
          */
          assert(cadr(identity).is<closure>());
          assert(cddr(identity).is<syntactic_environment>());

          return compile(Environment().apply(cadr(identity),
                                             expression,
                                             make<syntactic_environment>(bound_variables, second),
                                             cddr(identity)),
                         bound_variables,
                         free_variables,
                         continuation,
                         tail);
        }
        else if (cdr(identity).is<syntax>())
        {
          return cdr(identity).as<syntax>().compile(*this, cdr(expression), bound_variables, free_variables, continuation, tail);
        }

        assert(not cdr(identity).is_also<syntax>());
      }

      return syntax::call(*this, expression, bound_variables, free_variables, continuation, tail);
    }

    static auto core() -> auto const&
    {
      #define BINDING(NAME, SYNTAX) \
        make<absolute>(make_symbol(NAME), make<syntax>(NAME, syntax::SYNTAX, expander::SYNTAX))

      let static const core = make<syntactic_environment>(
        nullptr,
        list(BINDING("begin"                          , sequence                      ),
             BINDING("call-with-current-continuation!", call_with_current_continuation),
             BINDING("current"                        , current                       ),
             BINDING("define"                         , define                        ),
             BINDING("define-syntax"                  , define_syntax                 ),
             BINDING("if"                             , conditional                   ),
             BINDING("implementation-dependent"       , implementation_dependent      ),
             BINDING("include"                        , include                       ),
             BINDING("include-case-insensitive"       , include_case_insensitive      ),
             BINDING("install"                        , install                       ),
             BINDING("lambda"                         , lambda                        ),
             BINDING("let-syntax"                     , let_syntax                    ),
             BINDING("letrec"                         , letrec                        ),
             BINDING("letrec-syntax"                  , letrec_syntax                 ),
             BINDING("quote"                          , quote                         ),
             BINDING("quote-syntax"                   , quote_syntax                  ),
             BINDING("set!"                           , set                           )));

      #undef BINDING

      return core;
    }

    inline auto define(object const& variable, object const& value = undefined) -> void
    {
      assert(identify(variable, nullptr, nullptr).template is<absolute>());
      cdr(identify(variable, nullptr, nullptr)) = value;
    }

    template <typename T, typename... Ts>
    inline auto define(std::string const& name, Ts&&... xs) -> void
    {
      if constexpr (std::is_base_of_v<describable, T>)
      {
        return define(make_symbol(name), make<T>(name, std::forward<decltype(xs)>(xs)...));
      }
      else
      {
        return define(make_symbol(name), make<T>(std::forward<decltype(xs)>(xs)...));
      }
    }

    template <template <typename...> typename Deducer, typename... Ts>
    inline auto define(Ts&&... xs) -> decltype(auto)
    {
      return define<typename Deducer<Ts...>::type>(std::forward<decltype(xs)>(xs)...);
    }

    inline auto expand(object const& expression,
                       object const& bound_variables = nullptr, // list of <formals>
                       object const& free_variables = nullptr) const -> object
    {
      if (not expression.is<pair>())
      {
        return expression;
      }
      else if (let const& identity = identify(car(expression), bound_variables, free_variables); identity.is<absolute>())
      {
        if (cdr(identity).is<transformer>())
        {
          /*
             Scheme programs can define and use new derived expression types,
             called macros. Program-defined expression types have the syntax

               (<keyword> <datum>...)

             where <keyword> is an identifier that uniquely determines the
             expression type. This identifier is called the syntactic keyword,
             or simply keyword, of the macro. The number of the <datum>s, and
             their syntax, depends on the expression type.

             Each instance of a macro is called a use of the macro. The set of
             rules that specifies how a use of a macro is transcribed into a
             more primitive expression is called the transformer of the macro.
          */
          assert(cadr(identity).is<closure>());
          assert(cddr(identity).is<syntactic_environment>());

          return expand(Environment().apply(cadr(identity),
                                            expression,
                                            make<syntactic_environment>(bound_variables, second),
                                            cddr(identity)),
                        bound_variables,
                        free_variables);
        }
        else if (cdr(identity).is<syntax>())
        {
          return cdr(identity).as<syntax>().expand(*this,
                                                   expression,
                                                   bound_variables,
                                                   free_variables);
        }
      }

      return expander::call(*this, expression, bound_variables, free_variables);
    }

    inline auto identify(object const& variable,
                         object const& bound_variables,
                         object const& free_variables) const -> object
    {
      if (not variable.is_also<identifier>())
      {
        return f;
      }
      else if (let const& x = assq(variable, free_variables); x != f)
      {
        return cdr(x).as<syntactic_environment>().inject(car(x), bound_variables);
      }
      else
      {
        auto i = 0;

        for (auto outer = bound_variables; outer.is<pair>(); ++i, outer = cdr(outer))
        {
          auto j = 0;

          for (auto inner = outer.is<pair>() ? car(outer) : unit; not inner.is<null>(); ++j, inner = inner.is<pair>() ? cdr(inner) : unit)
          {
            if (inner.is<pair>())
            {
              if (car(inner).is<absolute>() and eq(caar(inner), variable))
              {
                return car(inner);
              }
              else if (eq(car(inner), variable))
              {
                return make<relative>(make<std::int32_t>(i), make<std::int32_t>(j));
              }
            }
            else if (inner.is_also<identifier>() and eq(inner, variable))
            {
              return make<variadic>(make<std::int32_t>(i), make<std::int32_t>(j));
            }
          }
        }

        if (variable.is<syntactic_closure>())
        {
          return car(variable).as<syntactic_environment>()
                              .identify(cddr(variable),
                                        unify(caar(variable), bound_variables),
                                        nullptr);
        }
        else
        {
          return assq(variable, second);
        }
      }
    }

    inline auto identify(object const& variable,
                         object const& bound_variables,
                         object const& free_variables)
    {
      if (not variable.is_also<identifier>())
      {
        return f;
      }
      else if (let const& identity = std::as_const(*this).identify(variable, bound_variables, free_variables); identity != f)
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
        return car(second = cons(make<absolute>(variable, undefined), second));
      }
    }

    inline auto inject(object const& free_variable,
                       object const& bound_variables) -> object
    {
      return identify(free_variable,
                      unify(first /* bound-variables */,
                            bound_variables),
                      second /* free-variables */);
    }

    static auto rename(std::string const& variable)
    {
      return make<syntactic_closure>(core(), cons(nullptr, make_symbol(variable)));
    }

    inline auto sweep(object const& form,
                      object const& bound_variables,
                      object const& free_variables,
                      object const& binding_specs = nullptr) const -> pair
    {
      if (form.is<pair>() and car(form).is<pair>())
      {
        if (let const& identity = identify(caar(form), bound_variables, free_variables); identity.is<absolute>())
        {
          if (let const& value = cdr(identity); value.is<transformer>())
          {
            return sweep(cons(Environment().apply(cadr(identity), // <closure>
                                                  car(form),
                                                  make<syntactic_environment>(bound_variables, second), // use-env
                                                  cddr(identity)), // mac-env
                              cdr(form)),
                         bound_variables,
                         free_variables,
                         binding_specs);
          }
          else if (value.is<syntax>())
          {
            if (auto const& name = value.as<syntax>().name; name == "begin")
            {
              return sweep(append(cdar(form), cdr(form)),
                           bound_variables,
                           free_variables,
                           binding_specs);
            }
            else if (name == "define") // <form> = ((define ...) <definition or expression>*)
            {
              if (let const& definition = car(form); cadr(definition).is<pair>()) // <form> = ((define (<variable> . <formals>) <body>) <definition or expression>*)
              {
                return sweep(cdr(form),
                             bound_variables,
                             free_variables,
                             cons(list(caadr(definition), // <variable>
                                       cons(rename("lambda"),
                                            cdadr(definition), // <formals>
                                            cddr(definition))), // <body>
                                  binding_specs));
              }
              else // <form> = ((define <variable> <expression>) <definition or expression>*)
              {
                return sweep(cdr(form),
                             bound_variables,
                             free_variables,
                             cons(cdr(definition), binding_specs));
              }
            }
            else if (name == "define-syntax") // <form> = ((define-syntax <keyword> <transformer spec>) <definition or expression>*)
            {
              return sweep(cdr(form),
                           bound_variables,
                           free_variables,
                           cons(list(make<absolute>(cadar(form), // <keyword>
                                                    caddar(form))), // <transformer spec>
                                binding_specs));
            }
          }
        }
      }

      return pair(binding_specs, form);
    }

    static auto unify(object const& a, object const& b) -> object
    {
      /*
         Consider the following case where an expression that uses a local
         macro is given:

           (let ((x 'outer))
             (let-syntax ((m (sc-macro-transformer
                               (lambda (form environment)
                                 'x))))
               (let ((x 'inner))
                 (m))))

         Where, the bound variables that the syntactic closure returned by
         sc-macro-transformer encloses are ((x)), and the bound variables when
         using the local macro m are ((x) (m) (x)).

         The result of the expansion of local macro m must be a reference to
         the local variable x that binds the symbol "outer" and not the one
         that binds the symbol "inner". That is, the operand of the relative
         loading instruction resulting from the expansion of the local macro m
         must be de Bruijn index (2 . 0).

         However, since syntactic_environment::identify searches bound
         variables from inside to outside to create de Bruijn index, it
         straightforwardly uses bound variables ((x) (m) (x)) when using local
         macro m would result in index (0 . 0).

         By searching for the common tail of the two bound variables and cons a
         dummy environment in front of the list to match the length of the
         longer bound variables, we can create bound variables that lead to an
         appropriate de Bruijn index. In the example above, this is (() ()
         (x)).
      */
      let xs = longest_common_tail(a, b);

      assert(length(xs) <= std::min(length(a), length(b)));

      for (auto offset = std::max(length(a), length(b)) - length(xs); 0 < offset; --offset)
      {
        xs = cons(nullptr, xs);
      }

      return xs;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP
