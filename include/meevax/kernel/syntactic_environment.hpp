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

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/conditional_expand.hpp>
#include <meevax/kernel/describable.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/include.hpp>
#include <meevax/kernel/transformer.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Environment>
  struct syntactic_environment : public virtual pair // (<bound-variables> . <free-variables>)
  {
    struct renamer
    {
      virtual auto operator ()(let const& form) -> object
      {
        return form;
      }
    }
    static inline default_rename {};

    struct syntactic_closure : public identifier
    {
      let environment, free_names, form;

      explicit syntactic_closure(let const& environment,
                                 let const& free_names,
                                 let const& form)
        : environment { environment }
        , free_names  { free_names }
        , form        { form }
      {
        assert(environment.is<syntactic_environment>());
      }

      auto expand(let const& bound_variables, renamer & inject)
      {
        struct local_renamer : public renamer
        {
          syntactic_closure const* enclosure;

          renamer & inject;

          let renamings;

          explicit local_renamer(syntactic_closure const* enclosure, renamer & inject)
            : enclosure { enclosure }
            , inject    { inject }
          {}

          auto rename(let const& form, let const& free_names) -> object
          {
            if (form.is<pair>()) // is <formals>
            {
              return cons(rename(car(form), unit),
                          rename(cdr(form), unit)); // Disable injection when renaming formals.
            }
            else if (form.is<symbol>())
            {
              if (let const& free_name = memq(form, free_names); free_name != f)
              {
                return inject(form);
              }
              else if (let const& renaming = assq(form, renamings); renaming != f)
              {
                return cdr(renaming);
              }
              else
              {
                return cdar(renamings = alist_cons(form,
                                                   make<syntactic_closure>(enclosure->environment, unit, form),
                                                   renamings));
              }
            }
            else
            {
              return form;
            }
          }

          auto operator ()(let const& form) -> object override
          {
            return rename(form, enclosure->free_names);
          }
        };

        auto rename = local_renamer(this, inject);

        return environment.as<syntactic_environment>().expand(form,
                                                              unify(car(environment), bound_variables),
                                                              rename);
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
        return x.form.template is_also<identifier>() and
               y.form.template is_also<identifier>() and
               eqv(x.environment.template as<syntactic_environment>().identify(x.form, car(x.environment)),
                   y.environment.template as<syntactic_environment>().identify(y.form, car(y.environment)));
      }

      friend auto operator <<(std::ostream & os, syntactic_closure const& datum) -> std::ostream &
      {
        return os << underline(datum.form);
      }
    };

    struct syntax : public describable
    {
      auto (*expand)(syntactic_environment const&,
                     object const& expression,
                     object const& bound_variables,
                     renamer &) -> object;

      auto (*generate)(syntactic_environment &,
                       object const& /* expression      */,
                       object const& /* bound_variables */,
                       object const& /* continuation    */,
                       bool          /* tail            */) -> object;

      static inline std::unordered_map<pair const*, object> contexts;

      template <typename Expander, typename Generator>
      explicit syntax(std::string const& name, Expander const& expand, Generator const& generate)
        : describable { name }
        , expand { expand }
        , generate { generate }
      {}

      struct constructor : private object
      {
        explicit constructor(let const& expression)
          : object { expression }
        {}

        template <typename... Ts>
        auto cons(let const& a,
                  let const& b, Ts&&... xs) const
        {
          auto cons2 = [&](let const& a, let const& b)
          {
            let const& x = meevax::cons(a, b);
            contexts[x.get()] = *this;
            return x;
          };

          if constexpr (0 < sizeof...(Ts))
          {
            return cons2(a, cons(b, std::forward<decltype(xs)>(xs)...));
          }
          else
          {
            return cons2(a, b);
          }
        }

        template <typename... Ts>
        auto list(Ts&&... xs) const -> decltype(auto)
        {
          return cons(std::forward<decltype(xs)>(xs)..., unit);
        }
      };

      #define CONS typename syntax::constructor(expression).cons

      #define LIST typename syntax::constructor(expression).list

      friend auto operator <<(std::ostream & os, syntax const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << green("syntax ") << datum.name << magenta(")");
      }
    };

    struct expander
    {
      #define EXPANDER(NAME)                                                   \
      auto NAME([[maybe_unused]] syntactic_environment const& expander,        \
                                 object const& expression,                     \
                [[maybe_unused]] object const& bound_variables,                \
                [[maybe_unused]] renamer & rename) -> object

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
        return CONS(expander.expand(car(expression),
                                    bound_variables,
                                    rename),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            rename));
      }

      static EXPANDER(operand)
      {
        if (expression.is<pair>())
        {
          return CONS(expander.expand(car(expression),
                                      bound_variables,
                                      rename),
                      operand(expander,
                              cdr(expression),
                              bound_variables,
                              rename));
        }
        else
        {
          return expander.expand(expression, bound_variables, rename);
        }
      }

      static EXPANDER(lambda)
      {
        let const& formals = rename(cadr(expression));

        return CONS(rename(car(expression)) /* lambda */,
                    CONS(formals,
                         body(expander,
                              cddr(expression),
                              cons(formals, bound_variables),
                              rename)));
      }

      static EXPANDER(body)
      {
        if (auto [binding_specs, sequence] = expander.sweep(expression, bound_variables); binding_specs)
        {
          /*
             (letrec* <binding specs> <sequence>)

                 => ((lambda <variables> <assignments> <sequence>)
                     <dummy 1> ... <dummy n>)

             where <binding specs> = ((<variable 1> <initial 1>) ...
                                      (<variable n> <initial n>))
          */
          let formals = unit;

          let body = sequence;

          for (let const& binding_spec : binding_specs) // The order of the list `binding_specs` returned from the function `sweep` is the reverse of the definition order.
          {
            let const& variable = car(binding_spec);

            formals = cons(variable, formals);

            if (not variable.is<absolute>()) // The binding-spec is not an internal syntax definition.
            {
              body = CONS(CONS(corename("set!"), binding_spec), body);
            }
          }

          let const current_environment = make<syntactic_environment>(cons(formals, bound_variables),
                                                                      expander.second);

          for (let & formal : formals)
          {
            if (formal.is<absolute>())
            {
              cdr(formal) = make<transformer>(Environment().execute(current_environment.as<syntactic_environment>().compile(cdr(formal) /* <transformer spec> */)),
                                              current_environment);
            }
          }

          return expander.expand(LIST(CONS(CONS(corename("lambda"),
                                                formals,
                                                body),
                                           make_list(length(binding_specs), unit))),
                                 bound_variables,
                                 rename);
        }
        else if (sequence.template is<pair>())
        {
          return CONS(expander.expand(car(sequence),
                                      bound_variables,
                                      rename),
                      body(expander,
                           cdr(sequence),
                           bound_variables,
                           rename));
        }
        else
        {
          return expander.expand(sequence, bound_variables, rename);
        }
      }

      static EXPANDER(conditional)
      {
        return CONS(rename(car(expression)),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            rename));
      }

      static EXPANDER(set)
      {
        return CONS(rename(car(expression)),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            rename));
      }

      static EXPANDER(include)
      {
        return expander.expand(CONS(corename("begin"),
                                    meevax::include(cadr(expression))),
                               bound_variables,
                               rename);
      }

      static EXPANDER(include_case_insensitive)
      {
        return expander.expand(CONS(corename("begin"),
                                    meevax::include(cadr(expression), false)),
                               bound_variables,
                               rename);
      }

      static EXPANDER(conditional_expand)
      {
        return expander.expand(CONS(corename("begin"),
                                    meevax::conditional_expand(cdr(expression))),
                               bound_variables,
                               rename);
      }

      static EXPANDER(letrec)
      {
        let const extended_bound_variables = cons(map(car, cadr(expression)), bound_variables);

        return CONS(car(expression),
                    map([&](let const& binding)
                        {
                          return LIST(car(binding),
                                      expander.expand(cadr(binding),
                                                      extended_bound_variables,
                                                      rename));
                        },
                        cadr(expression)),
                    body(expander,
                         cddr(expression),
                         extended_bound_variables,
                         rename));
      }

      static EXPANDER(sequence)
      {
        if (expression.is<pair>())
        {
          return CONS(expander.expand(car(expression),
                                      bound_variables,
                                      rename),
                      sequence(expander,
                               cdr(expression),
                               bound_variables,
                               rename));
        }
        else
        {
          return expander.expand(expression, bound_variables, rename);
        }
      }

      static EXPANDER(let_syntax)
      {
        let const current_environment = make<syntactic_environment>(bound_variables, expander.second);

        auto formal = [&](let const& syntax_spec)
        {
          return make<absolute>(car(syntax_spec) /* keyword */,
                                make<transformer>(Environment().execute(current_environment.as<syntactic_environment>().compile(cadr(syntax_spec) /* transformer spec */)),
                                                  current_environment));
        };

        let const formals = map(formal, cadr(expression));

        return expander.expand(LIST(CONS(corename("lambda"),
                                         formals,
                                         cddr(expression) /* body */)),
                               bound_variables,
                               rename);
      }

      static EXPANDER(letrec_syntax)
      {
        let current_environment = make<syntactic_environment>(bound_variables, expander.second);

        auto formal = [&](let const& syntax_spec)
        {
          return make<absolute>(car(syntax_spec) /* keyword */,
                                make<transformer>(Environment().execute(current_environment.as<syntactic_environment>().compile(cadr(syntax_spec) /* transformer spec */)),
                                                  current_environment));
        };

        let const formals = map(formal, cadr(expression));

        car(current_environment) = cons(formals, bound_variables);

        return expander.expand(LIST(CONS(corename("lambda"),
                                         formals,
                                         cddr(expression) /* body */)),
                               bound_variables,
                               rename);
      }

      static EXPANDER(define)
      {
        if (bound_variables.is<null>())
        {
          if (cadr(expression).is<pair>()) // (define (<variable> . <formals>) <body>)
          {
            return LIST(rename(car(expression)),
                        caadr(expression) /* variable */,
                        expander.expand(CONS(corename("lambda"),
                                             cdadr(expression) /* formals */,
                                             cddr(expression) /* body */),
                                        bound_variables,
                                        rename));
          }
          else // (define <variable> <expression>)
          {
            return CONS(rename(car(expression)),
                        cadr(expression),
                        cddr(expression) ? LIST(expander.expand(caddr(expression),
                                                                bound_variables,
                                                                rename))
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
        return LIST(rename(car(expression)),
                    cadr(expression),
                    expander.expand(caddr(expression),
                                    bound_variables,
                                    rename));
      }

      static EXPANDER(call_with_current_continuation)
      {
        return CONS(rename(car(expression)),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            rename));
      }

      static EXPANDER(current)
      {
        return CONS(rename(car(expression)),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            rename));
      }

      static EXPANDER(install)
      {
        return CONS(rename(car(expression)),
                    operand(expander,
                            cdr(expression),
                            bound_variables,
                            rename));
      }

      #undef EXPANDER
    };

    struct generator
    {
      #define GENERATOR(NAME)                                                  \
      auto NAME([[maybe_unused]] syntactic_environment & generator,            \
                [[maybe_unused]] object const& expression,                     \
                [[maybe_unused]] object const& bound_variables,                \
                [[maybe_unused]] object const& continuation,                   \
                [[maybe_unused]] bool tail = false) -> object

      static GENERATOR(quote)
      {
        return CONS(make(instruction::load_constant), car(expression).is<syntactic_closure>() ? car(expression).as<syntactic_closure>().form
                                                                                              : car(expression),
                    continuation);
      }

      static GENERATOR(quote_syntax)
      {
        return CONS(make(instruction::load_constant), car(expression),
                    continuation);
      }

      static GENERATOR(call)
      {
        return operand(generator,
                       cdr(expression),
                       bound_variables,
                       generator.generate(car(expression),
                                          bound_variables,
                                          tail ? LIST(make(instruction::tail_call))
                                               : CONS(make(instruction::call), continuation)));
      }

      static GENERATOR(operand)
      {
        if (expression.is<pair>())
        {
          return operand(generator,
                         cdr(expression),
                         bound_variables,
                         generator.generate(car(expression),
                                            bound_variables,
                                            CONS(make(instruction::cons),
                                                 continuation)));
        }
        else
        {
          return generator.generate(expression, bound_variables, continuation);
        }
      }

      static GENERATOR(lambda)
      {
        return CONS(make(instruction::load_closure),
                    body(generator,
                         cdr(expression),
                         cons(car(expression), bound_variables), // Extend scope.
                         LIST(make(instruction::return_))),
                    continuation);
      }

      static GENERATOR(body)
      {
        if (cdr(expression).template is<null>())
        {
          return generator.generate(car(expression),
                                    bound_variables,
                                    continuation,
                                    true);
        }
        else
        {
          return generator.generate(car(expression),
                                    bound_variables,
                                    CONS(make(instruction::drop),
                                         body(generator,
                                              cdr(expression),
                                              bound_variables,
                                              continuation)),
                                    false);
        }
      }

      static GENERATOR(conditional)
      {
        if (tail)
        {
          assert(lexical_cast<std::string>(continuation) == "(return)");

          return generator.generate(car(expression), // <test>
                                    bound_variables,
                                    LIST(make(instruction::tail_select),
                                         generator.generate(cadr(expression),
                                                            bound_variables,
                                                            continuation,
                                                            tail),
                                         cddr(expression) ? generator.generate(caddr(expression),
                                                                               bound_variables,
                                                                               continuation,
                                                                               tail)
                                                          : LIST(make(instruction::load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                                 make(instruction::return_))));
        }
        else
        {
          return generator.generate(car(expression), // <test>
                                    bound_variables,
                                    CONS(make(instruction::select),
                                         generator.generate(cadr(expression),
                                                            bound_variables,
                                                            LIST(make(instruction::join))),
                                         cddr(expression) ? generator.generate(caddr(expression),
                                                                               bound_variables,
                                                                               LIST(make(instruction::join)))
                                                          : LIST(make(instruction::load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                                 make(instruction::join)),
                                         continuation));
        }
      }

      static GENERATOR(set)
      {
        if (let const& identity = generator.identify(car(expression), bound_variables); identity.is<relative>())
        {
          return generator.generate(cadr(expression),
                                    bound_variables,
                                    CONS(make(instruction::store_relative), identity,
                                         continuation));
        }
        else if (identity.is<variadic>())
        {
          return generator.generate(cadr(expression),
                                    bound_variables,
                                    CONS(make(instruction::store_variadic), identity,
                                         continuation));
        }
        else
        {
          assert(identity.is<absolute>());

          return generator.generate(cadr(expression),
                                    bound_variables,
                                    CONS(make(instruction::store_absolute), identity,
                                         continuation));
        }
      }

      static constexpr auto include = nullptr;

      static constexpr auto include_case_insensitive = nullptr;

      static constexpr auto conditional_expand = nullptr;

      static GENERATOR(letrec)
      {
        assert(not tail or lexical_cast<std::string>(continuation) == "(return)");

        let const formals = map(car, car(expression));

        return CONS(make(instruction::dummy),
                    operand(generator,
                            map(cadr, car(expression)),
                            cons(formals, bound_variables),
                            lambda(generator,
                                   cons(formals, cdr(expression)), // (<formals> <body>)
                                   bound_variables,
                                   tail ? LIST(make(instruction::tail_letrec))
                                        : CONS(make(instruction::letrec), continuation))));
      }

      static GENERATOR(sequence)
      {
        if (cdr(expression).is<null>()) // is tail sequence
        {
          return generator.generate(car(expression),
                                    bound_variables,
                                    continuation,
                                    tail);
        }
        else
        {
          /*
             The top-level sequential expression may contain macro definitions.
             In that case, the macro definition must be compiled before the
             macro is used (the evaluation order of function arguments in C++
             is not specified, but in most environments they are evaluated from
             right to left). Therefore, the first expression is compiled
             separately and then combined with the compiled result of the
             remaining expressions by append.
          */
          let const& head = generator.generate(car(expression), // Head expression or definition
                                               bound_variables,
                                               unit);
          return append(head,
                        CONS(make(instruction::drop), // Pop result of head expression
                             sequence(generator,
                                      cdr(expression), // Rest expression or definitions
                                      bound_variables,
                                      continuation,
                                      tail)));
        }
      }

      static constexpr auto let_syntax = nullptr;

      static constexpr auto letrec_syntax = nullptr;

      static GENERATOR(define)
      {
        assert(bound_variables.is<null>()); // This has been checked on previous passes.

        assert(not car(expression).is<pair>()); // This has been checked on previous passes.

        return generator.generate(cdr(expression) ? cadr(expression) : unspecified,
                                  bound_variables,
                                  CONS(make(instruction::store_absolute), generator.identify(car(expression), bound_variables),
                                       continuation));
      }

      static GENERATOR(define_syntax)
      {
        let identity = generator.identify(car(expression), unit);

        cdr(identity) = make<transformer>(Environment().execute(generator.generate(cadr(expression),
                                                                                   bound_variables)),
                                          make<syntactic_environment>(bound_variables,
                                                                      generator.second));

        return CONS(make(instruction::load_constant), unspecified,
                    continuation);
      }

      static GENERATOR(call_with_current_continuation)
      {
        assert(expression.is<pair>());
        assert(cdr(expression).is<null>());

        return CONS(make(instruction::load_continuation),
                    continuation,
                    generator.generate(car(expression),
                                       bound_variables,
                                       LIST(make(instruction::tail_call)), // The first argument passed to call-with-current-continuation must be called via a tail call.
                                       tail));
      }

      static GENERATOR(current)
      {
        return CONS(make(instruction::current), car(expression),
                    continuation);
      }

      static GENERATOR(install)
      {
        return generator.generate(cadr(expression),
                                  bound_variables,
                                  CONS(make(instruction::install), car(expression),
                                       continuation));
      }

      #undef GENERATOR
    };

    using pair::pair;

    template <typename... Ts>
    inline auto compile(object const& expression,
                        object const& bound_variables, Ts&&... xs) -> decltype(auto)
    {
      return generate(expand(expression,
                             bound_variables,
                             default_rename),
                      bound_variables,
                      std::forward<decltype(xs)>(xs)...);
    }

    inline auto compile(object const& expression) -> decltype(auto)
    {
      return compile(expression, first);
    }

    static auto core() -> auto const&
    {
      #define BIND(NAME, SYNTAX) \
        make<absolute>(make_symbol(NAME), make<syntax>(NAME, expander::SYNTAX, generator::SYNTAX))

      let static const core = make<syntactic_environment>(
        unit,
        list(BIND("begin"                          , sequence                      ),
             BIND("call-with-current-continuation!", call_with_current_continuation),
             BIND("conditional-expand"             , conditional_expand            ),
             BIND("current"                        , current                       ),
             BIND("define"                         , define                        ),
             BIND("define-syntax"                  , define_syntax                 ),
             BIND("if"                             , conditional                   ),
             BIND("include"                        , include                       ),
             BIND("include-case-insensitive"       , include_case_insensitive      ),
             BIND("install"                        , install                       ),
             BIND("lambda"                         , lambda                        ),
             BIND("let-syntax"                     , let_syntax                    ),
             BIND("letrec"                         , letrec                        ),
             BIND("letrec-syntax"                  , letrec_syntax                 ),
             BIND("quote"                          , quote                         ),
             BIND("quote-syntax"                   , quote_syntax                  ),
             BIND("set!"                           , set                           )));

      #undef BIND

      return core;
    }

    static auto corename(std::string const& variable)
    {
      return make<syntactic_closure>(core(), unit, make_symbol(variable));
    }

    inline auto define(object const& variable, object const& value = undefined) -> void
    {
      assert(identify(variable, unit).template is<absolute>());
      cdr(identify(variable, unit)) = value;
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
                       object const& bound_variables,
                       renamer & rename) const -> object try
    {
      if (not expression.is<pair>())
      {
        if (expression.is<syntactic_closure>())
        {
          if (let const& identity = identify(expression, bound_variables); identity == f)
          {
            return expression.as<syntactic_closure>().expand(bound_variables, rename);
          }
        }

        return expression.is_also<identifier>() ? rename(expression) : expression;
      }
      else if (let const& identity = identify(car(expression), bound_variables); identity.is<absolute>())
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

          let const transformed = Environment().apply(cadr(identity),
                                                      expression,
                                                      make<syntactic_environment>(bound_variables, second),
                                                      cddr(identity));

          syntax::contexts[transformed.get()] = expression;

          return expand(transformed, bound_variables, rename);
        }
        else if (cdr(identity).is<syntax>())
        {
          return cdr(identity).as<syntax>().expand(*this, expression, bound_variables, rename);
        }
      }

      return expander::call(*this, expression, bound_variables, rename);
    }
    catch (error & e)
    {
      e.detail(error::in::expanding, expression).raise();
      return unspecified;
    }

    inline auto generate(object const& expression,
                         object const& bound_variables,
                         object const& continuation = list(make(instruction::stop)),
                         bool tail = false) -> object try
    {
      if (not expression.is<pair>())
      {
        if (expression.is_also<identifier>())
        {
          assert(expression.is<symbol>() or expression.is<syntactic_closure>());

          if (let const& identity = identify(expression, bound_variables); identity.is<relative>())
          {
            return CONS(make(instruction::load_relative), identity, continuation);
          }
          else if (identity.is<variadic>())
          {
            return CONS(make(instruction::load_variadic), identity, continuation);
          }
          else
          {
            assert(identity.is<absolute>());
            return CONS(make(instruction::load_absolute), identity, continuation);
          }
        }
        else // is <self-evaluating>
        {
          return CONS(make(instruction::load_constant), expression, continuation);
        }
      }
      else if (let const& identity = std::as_const(*this).identify(car(expression), bound_variables); identity.is<absolute>() and cdr(identity).is<syntax>())
      {
        return cdr(identity).as<syntax>().generate(*this, cdr(expression), bound_variables, continuation, tail);
      }
      else
      {
        return generator::call(*this, expression, bound_variables, continuation, tail);
      }
    }
    catch (error & e)
    {
      e.detail(error::in::generating, expression).raise();
      return unspecified;
    }

    inline auto identify(object const& variable,
                         object const& bound_variables) const -> object
    {
      if (not variable.is_also<identifier>())
      {
        return f;
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

        if (variable.is<syntactic_closure>()) // Resolve alias
        {
          return variable.as<syntactic_closure>()
                         .environment
                         .template as<syntactic_environment>()
                         .identify(variable.as<syntactic_closure>().form,
                                   unify(car(variable.as<syntactic_closure>().environment),
                                         bound_variables));
        }
        else
        {
          return assq(variable, second);
        }
      }
    }

    inline auto identify(object const& variable,
                         object const& bound_variables)
    {
      if (not variable.is_also<identifier>())
      {
        return f;
      }
      else if (let const& identity = std::as_const(*this).identify(variable, bound_variables); identity != f)
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
        assert(not variable.is<syntactic_closure>());

        return car(second = cons(make<absolute>(variable, undefined), second));
      }
    }

    inline auto sweep(object const& form,
                      object const& bound_variables,
                      object const& binding_specs = unit) const -> pair
    {
      if (form.is<pair>() and car(form).is<pair>())
      {
        if (let const& identity = identify(caar(form), bound_variables); identity.is<absolute>())
        {
          if (let const& value = cdr(identity); value.is<transformer>())
          {
            return sweep(cons(Environment().apply(cadr(identity), // <closure>
                                                  car(form),
                                                  make<syntactic_environment>(bound_variables, second), // use-env
                                                  cddr(identity)), // mac-env
                              cdr(form)),
                         bound_variables,
                         binding_specs);
          }
          else if (value.is<syntax>())
          {
            if (auto const& name = value.as<syntax>().name; name == "begin")
            {
              return sweep(append(cdar(form), cdr(form)),
                           bound_variables,
                           binding_specs);
            }
            else if (name == "define") // <form> = ((define ...) <definition or expression>*)
            {
              if (let const& definition = car(form); cadr(definition).is<pair>()) // <form> = ((define (<variable> . <formals>) <body>) <definition or expression>*)
              {
                return sweep(cdr(form),
                             bound_variables,
                             cons(list(caadr(definition), // <variable>
                                       cons(corename("lambda"),
                                            cdadr(definition), // <formals>
                                            cddr(definition))), // <body>
                                  binding_specs));
              }
              else // <form> = ((define <variable> <expression>) <definition or expression>*)
              {
                return sweep(cdr(form),
                             bound_variables,
                             cons(cdr(definition), binding_specs));
              }
            }
            else if (name == "define-syntax") // <form> = ((define-syntax <keyword> <transformer spec>) <definition or expression>*)
            {
              return sweep(cdr(form),
                           bound_variables,
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
        xs = cons(unit, xs);
      }

      return xs;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP
