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

namespace meevax::inline kernel
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
        struct implicit_renamer : public renamer
        {
          syntactic_closure const* enclosure;

          let bound_variables;

          renamer & inject;

          let dictionary;

          explicit implicit_renamer(syntactic_closure const* enclosure,
                                    let const& bound_variables,
                                    renamer & inject)
            : enclosure       { enclosure }
            , bound_variables { bound_variables }
            , inject          { inject }
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
              if (let const& free_name = memq(form, free_names); free_name != f or eq(car(enclosure->environment), bound_variables))
              {
                return inject(form);
              }
              else if (let const& renaming = assq(form, dictionary); renaming != f)
              {
                return cdr(renaming);
              }
              else
              {
                return cdar(dictionary = alist_cons(form,
                                                    make<syntactic_closure>(enclosure->environment, unit, form),
                                                    dictionary));
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

        auto implicit_rename = implicit_renamer(this, bound_variables, inject);

        return environment.as<syntactic_environment>().expand(form, unify(bound_variables), implicit_rename);
      }

      auto identify(let const& bound_variables)
      {
        return environment.as_const<syntactic_environment>().identify(form, unify(bound_variables));
      }

      auto unify(object const& bound_variables) -> object
      {
        /*
           The bound variables of a macro defined by define-syntax are always
           empty. On the other hand, the bound variables of local macros
           defined by let-syntax and letrec-syntax have zero or more levels.
        */
        assert(length(car(environment)) <= length(bound_variables));


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
           sc-macro-transformer encloses are ((x)), and the bound variables
           when using the local macro m are ((x) (m) (x)).

           The result of the expansion of local macro m must be a reference to
           the local variable x that binds the symbol "outer" and not the one
           that binds the symbol "inner". That is, the operand of the relative
           loading instruction resulting from the expansion of the local macro
           m must be de Bruijn index (2 . 0).

           However, since syntactic_environment::identify searches bound
           variables from inside to outside to create de Bruijn index, it
           straightforwardly uses bound variables ((x) (m) (x)) when using
           local macro m would result in index (0 . 0).

           By searching for the common tail of the two bound variables and cons
           a dummy environment in front of the list to match the length of the
           longer bound variables, we can create bound variables that lead to
           an appropriate de Bruijn index. In the example above, this is (() ()
           (x)).
        */
        let xs = car(environment);

        for (auto offset = length(bound_variables) - length(car(environment)); 0 < offset; --offset)
        {
          xs = cons(unit, xs);
        }

        return xs;
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

    struct transformer : public virtual pair // (<closure> . <syntactic_environment>)
    {
      using pair::pair;

      auto transform(let const& form, let const& environment) const -> object
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
        assert(first.is<closure>());
        assert(second.is<syntactic_environment>());

        return Environment().apply(first, form, environment, second);
      }

      friend auto operator <<(std::ostream & os, transformer const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << green("transformer ") << faint("#;", &datum) << magenta(")");
      }
    };

    struct syntax : public describable
    {
      auto (*expand)(syntactic_environment const&,
                     object const& form,
                     object const& bound_variables,
                     renamer &) -> object;

      auto (*generate)(syntactic_environment &,
                       object const& /* form            */,
                       object const& /* bound_variables */,
                       object const& /* continuation    */,
                       bool          /* tail            */) -> object;

      template <typename Expander, typename Generator>
      explicit syntax(std::string const& name, Expander const& expand, Generator const& generate)
        : describable { name }
        , expand { expand }
        , generate { generate }
      {}

      friend auto operator <<(std::ostream & os, syntax const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << green("syntax ") << datum.name << magenta(")");
      }
    };

    struct expander
    {
      #define EXPANDER(NAME)                                                   \
      auto NAME([[maybe_unused]] syntactic_environment const& expander,        \
                                 object const& form,                           \
                [[maybe_unused]] object const& bound_variables,                \
                [[maybe_unused]] renamer & rename) -> object

      static EXPANDER(quote)
      {
        return form;
      }

      static EXPANDER(quote_syntax)
      {
        return form;
      }

      static EXPANDER(call)
      {
        return cons(expander.expand(car(form),
                                    bound_variables,
                                    rename),
                    operand(expander,
                            cdr(form),
                            bound_variables,
                            rename));
      }

      static EXPANDER(operand)
      {
        if (form.is<pair>())
        {
          return cons(expander.expand(car(form),
                                      bound_variables,
                                      rename),
                      operand(expander,
                              cdr(form),
                              bound_variables,
                              rename));
        }
        else
        {
          return expander.expand(form, bound_variables, rename);
        }
      }

      static EXPANDER(lambda)
      {
        let const& formals = rename(cadr(form));

        return cons(rename(car(form)) /* lambda */,
                    cons(formals,
                         body(expander,
                              cddr(form),
                              cons(formals, bound_variables),
                              rename)));
      }

      static EXPANDER(body)
      {
        let current_environment = make<syntactic_environment>(bound_variables, expander.second);

        if (auto [binding_specs, sequence] = expander.sweep(form, bound_variables, current_environment); binding_specs)
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
              body = cons(cons(corename("set!"), binding_spec), body);
            }
          }

          car(current_environment) = cons(formals, bound_variables);

          for (let & formal : formals)
          {
            if (formal.is<absolute>()) // is internal-sytnax-definition
            {
              cdr(formal) = make<transformer>(Environment().execute(current_environment.as<syntactic_environment>().compile(cdr(formal) /* <transformer spec> */)),
                                              current_environment);
            }
          }

          return expander.expand(list(cons(cons(corename("lambda"),
                                                formals,
                                                body),
                                           make_list(length(binding_specs), unit))),
                                 bound_variables,
                                 rename);
        }
        else if (sequence.template is<pair>())
        {
          return cons(expander.expand(car(sequence),
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
        return cons(rename(car(form)),
                    operand(expander,
                            cdr(form),
                            bound_variables,
                            rename));
      }

      static EXPANDER(set)
      {
        return cons(rename(car(form)),
                    operand(expander,
                            cdr(form),
                            bound_variables,
                            rename));
      }

      static EXPANDER(include)
      {
        return expander.expand(cons(corename("begin"),
                                    meevax::include(cadr(form))),
                               bound_variables,
                               rename);
      }

      static EXPANDER(include_case_insensitive)
      {
        return expander.expand(cons(corename("begin"),
                                    meevax::include(cadr(form), false)),
                               bound_variables,
                               rename);
      }

      static EXPANDER(conditional_expand)
      {
        return expander.expand(cons(corename("begin"),
                                    meevax::conditional_expand(cdr(form))),
                               bound_variables,
                               rename);
      }

      static EXPANDER(letrec)
      {
        let const extended_bound_variables = cons(map(car, cadr(form)), bound_variables);

        return cons(car(form),
                    map([&](let const& binding)
                        {
                          return list(car(binding),
                                      expander.expand(cadr(binding),
                                                      extended_bound_variables,
                                                      rename));
                        },
                        cadr(form)),
                    body(expander,
                         cddr(form),
                         extended_bound_variables,
                         rename));
      }

      static EXPANDER(sequence)
      {
        if (form.is<pair>())
        {
          return cons(expander.expand(car(form),
                                      bound_variables,
                                      rename),
                      sequence(expander,
                               cdr(form),
                               bound_variables,
                               rename));
        }
        else
        {
          return expander.expand(form, bound_variables, rename);
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

        let const formals = map(formal, cadr(form));

        return expander.expand(list(cons(corename("lambda"),
                                         formals,
                                         cddr(form) /* body */)),
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

        let const formals = map(formal, cadr(form));

        car(current_environment) = cons(formals, bound_variables);

        return expander.expand(list(cons(corename("lambda"),
                                         formals,
                                         cddr(form) /* body */)),
                               bound_variables,
                               rename);
      }

      static EXPANDER(define)
      {
        if (bound_variables.is<null>())
        {
          if (cadr(form).is<pair>()) // (define (<variable> . <formals>) <body>)
          {
            return list(rename(car(form)),
                        caadr(form) /* variable */,
                        expander.expand(cons(corename("lambda"),
                                             cdadr(form) /* formals */,
                                             cddr(form) /* body */),
                                        bound_variables,
                                        rename));
          }
          else // (define <variable> <expression>)
          {
            return cons(rename(car(form)),
                        cadr(form),
                        cddr(form) ? list(expander.expand(caddr(form),
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
        return list(rename(car(form)),
                    cadr(form),
                    expander.expand(caddr(form),
                                    bound_variables,
                                    rename));
      }

      static EXPANDER(call_with_current_continuation)
      {
        return cons(rename(car(form)),
                    operand(expander,
                            cdr(form),
                            bound_variables,
                            rename));
      }

      static EXPANDER(current)
      {
        return cons(rename(car(form)),
                    operand(expander,
                            cdr(form),
                            bound_variables,
                            rename));
      }

      static EXPANDER(install)
      {
        return cons(rename(car(form)),
                    operand(expander,
                            cdr(form),
                            bound_variables,
                            rename));
      }

      #undef EXPANDER
    };

    struct generator
    {
      #define GENERATOR(NAME)                                                  \
      auto NAME([[maybe_unused]] syntactic_environment & generator,            \
                [[maybe_unused]] object const& form,                           \
                [[maybe_unused]] object const& bound_variables,                \
                [[maybe_unused]] object const& continuation,                   \
                [[maybe_unused]] bool tail = false) -> object

      static GENERATOR(quote)
      {
        return cons(make(instruction::load_constant), car(form).is<syntactic_closure>() ? car(form).as<syntactic_closure>().form
                                                                                        : car(form),
                    continuation);
      }

      static GENERATOR(quote_syntax)
      {
        return cons(make(instruction::load_constant), car(form),
                    continuation);
      }

      static GENERATOR(call)
      {
        return operand(generator,
                       cdr(form),
                       bound_variables,
                       generator.generate(car(form),
                                          bound_variables,
                                          tail ? list(make(instruction::tail_call))
                                               : cons(make(instruction::call), continuation)));
      }

      static GENERATOR(operand)
      {
        if (form.is<pair>())
        {
          return operand(generator,
                         cdr(form),
                         bound_variables,
                         generator.generate(car(form),
                                            bound_variables,
                                            cons(make(instruction::cons),
                                                 continuation)));
        }
        else
        {
          return generator.generate(form, bound_variables, continuation);
        }
      }

      static GENERATOR(lambda)
      {
        return cons(make(instruction::load_closure),
                    body(generator,
                         cdr(form),
                         cons(car(form), bound_variables), // Extend scope.
                         list(make(instruction::return_))),
                    continuation);
      }

      static GENERATOR(body)
      {
        if (cdr(form).template is<null>())
        {
          return generator.generate(car(form),
                                    bound_variables,
                                    continuation,
                                    true);
        }
        else
        {
          return generator.generate(car(form),
                                    bound_variables,
                                    cons(make(instruction::drop),
                                         body(generator,
                                              cdr(form),
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

          return generator.generate(car(form), // <test>
                                    bound_variables,
                                    list(make(instruction::tail_select),
                                         generator.generate(cadr(form),
                                                            bound_variables,
                                                            continuation,
                                                            tail),
                                         cddr(form) ? generator.generate(caddr(form),
                                                                         bound_variables,
                                                                         continuation,
                                                                         tail)
                                                    : list(make(instruction::load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                           make(instruction::return_))));
        }
        else
        {
          return generator.generate(car(form), // <test>
                                    bound_variables,
                                    cons(make(instruction::select),
                                         generator.generate(cadr(form),
                                                            bound_variables,
                                                            list(make(instruction::join))),
                                         cddr(form) ? generator.generate(caddr(form),
                                                                         bound_variables,
                                                                         list(make(instruction::join)))
                                                    : list(make(instruction::load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                           make(instruction::join)),
                                         continuation));
        }
      }

      static GENERATOR(set)
      {
        assert(car(form).is_also<identifier>());

        if (let const& identity = generator.identify(car(form), bound_variables); identity.is<relative>())
        {
          return generator.generate(cadr(form),
                                    bound_variables,
                                    cons(make(instruction::store_relative), identity,
                                         continuation));
        }
        else if (identity.is<variadic>())
        {
          return generator.generate(cadr(form),
                                    bound_variables,
                                    cons(make(instruction::store_variadic), identity,
                                         continuation));
        }
        else
        {
          assert(identity.is<absolute>());

          return generator.generate(cadr(form),
                                    bound_variables,
                                    cons(make(instruction::store_absolute), identity,
                                         continuation));
        }
      }

      static constexpr auto include = nullptr;

      static constexpr auto include_case_insensitive = nullptr;

      static constexpr auto conditional_expand = nullptr;

      static GENERATOR(letrec)
      {
        assert(not tail or lexical_cast<std::string>(continuation) == "(return)");

        let const formals = map(car, car(form));

        return cons(make(instruction::dummy),
                    operand(generator,
                            map(cadr, car(form)),
                            cons(formals, bound_variables),
                            lambda(generator,
                                   cons(formals, cdr(form)), // (<formals> <body>)
                                   bound_variables,
                                   tail ? list(make(instruction::tail_letrec))
                                        : cons(make(instruction::letrec), continuation))));
      }

      static GENERATOR(sequence)
      {
        if (cdr(form).is<null>()) // is tail sequence
        {
          return generator.generate(car(form),
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
          let const& head = generator.generate(car(form), // Head expression or definition
                                               bound_variables,
                                               unit);
          return append(head,
                        cons(make(instruction::drop), // Pop result of head expression
                             sequence(generator,
                                      cdr(form), // Rest expression or definitions
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

        assert(not car(form).is<pair>()); // This has been checked on previous passes.

        assert(car(form).is_also<identifier>());

        return generator.generate(cdr(form) ? cadr(form) : unspecified,
                                  bound_variables,
                                  cons(make(instruction::store_absolute), generator.identify(car(form), bound_variables),
                                       continuation));
      }

      static GENERATOR(define_syntax)
      {
        assert(car(form).is_also<identifier>());

        let identity = generator.identify(car(form), unit);

        cdr(identity) = make<transformer>(Environment().execute(generator.generate(cadr(form),
                                                                                   bound_variables)),
                                          make<syntactic_environment>(bound_variables,
                                                                      generator.second));

        return cons(make(instruction::load_constant), unspecified,
                    continuation);
      }

      static GENERATOR(call_with_current_continuation)
      {
        assert(form.is<pair>());
        assert(cdr(form).is<null>());

        return cons(make(instruction::load_continuation),
                    continuation,
                    generator.generate(car(form),
                                       bound_variables,
                                       list(make(instruction::tail_call)), // The first argument passed to call-with-current-continuation must be called via a tail call.
                                       tail));
      }

      static GENERATOR(current)
      {
        return cons(make(instruction::current), car(form),
                    continuation);
      }

      static GENERATOR(install)
      {
        return generator.generate(cadr(form),
                                  bound_variables,
                                  cons(make(instruction::install), car(form),
                                       continuation));
      }

      #undef GENERATOR
    };

    using pair::pair;

    template <typename... Ts>
    inline auto compile(object const& form,
                        object const& bound_variables, Ts&&... xs) -> decltype(auto)
    {
      return generate(expand(form, bound_variables, default_rename),
                      bound_variables,
                      std::forward<decltype(xs)>(xs)...);
    }

    inline auto compile(object const& form) -> decltype(auto)
    {
      return compile(form, first);
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
      assert(variable.is_also<identifier>());
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

    inline auto expand(object const& form,
                       object const& bound_variables,
                       renamer & rename) const -> object try
    {
      if (not form.is<pair>())
      {
        if (form.is<syntactic_closure>())
        {
          assert(form.is_also<identifier>());

          if (let const& identity = identify(form, bound_variables); identity == f)
          {
            return form.as<syntactic_closure>().expand(bound_variables, rename);
          }
        }

        return form.is_also<identifier>() ? rename(form) : form;
      }
      else if (car(form).is_also<identifier>())
      {
        if (let const& identity = identify(car(form), bound_variables); identity.is<absolute>())
        {
          if (let const& value = cdr(identity); value.is<transformer>())
          {
            return expand(value.as<transformer>().transform(form, make<syntactic_environment>(bound_variables, second)),
                          bound_variables,
                          rename);
          }
          else if (value.is<syntax>())
          {
            return value.as<syntax>().expand(*this, form, bound_variables, rename);
          }
        }
      }

      return expander::call(*this, form, bound_variables, rename);
    }
    catch (error & e)
    {
      e.detail(error::in::expanding, form).raise();
      return unspecified;
    }

    inline auto generate(object const& form,
                         object const& bound_variables,
                         object const& continuation = list(make(instruction::stop)),
                         bool tail = false) -> object try
    {
      if (not form.is<pair>())
      {
        if (form.is_also<identifier>())
        {
          assert(form.is_also<identifier>());

          if (let const& identity = identify(form, bound_variables); identity.is<relative>())
          {
            return cons(make(instruction::load_relative), identity, continuation);
          }
          else if (identity.is<variadic>())
          {
            return cons(make(instruction::load_variadic), identity, continuation);
          }
          else
          {
            assert(identity.is<absolute>());
            return cons(make(instruction::load_absolute), identity, continuation);
          }
        }
        else // is <self-evaluating>
        {
          return cons(make(instruction::load_constant), form, continuation);
        }
      }
      else if (car(form).is_also<identifier>())
      {
        if (let const& identity = std::as_const(*this).identify(car(form), bound_variables); identity.is<absolute>() and cdr(identity).is<syntax>())
        {
          return cdr(identity).as<syntax>().generate(*this, cdr(form), bound_variables, continuation, tail);
        }
      }

      return generator::call(*this, form, bound_variables, continuation, tail);
    }
    catch (error & e)
    {
      e.detail(error::in::generating, form).raise();
      return unspecified;
    }

    inline auto identify(object const& variable,
                         object const& bound_variables) const -> object
    {
      assert(variable.is_also<identifier>());

      for (auto i = 0; let formals : bound_variables)
      {
        for (auto j = 0; not formals.is<null>(); formals = cdr(formals))
        {
          if (formals.is<pair>())
          {
            if (car(formals).is<absolute>() and eq(caar(formals), variable))
            {
              return car(formals);
            }
            else if (eq(car(formals), variable))
            {
              return make<relative>(make<std::int32_t>(i), make<std::int32_t>(j));
            }
          }
          else if (formals.is_also<identifier>() and eq(formals, variable))
          {
            return make<variadic>(make<std::int32_t>(i), make<std::int32_t>(j));
          }

          ++j;
        }

        ++i;
      }

      if (variable.is<syntactic_closure>() and
          variable.as<syntactic_closure>().form.template is_also<identifier>()) // if is an alias
      {
        return variable.as<syntactic_closure>().identify(bound_variables);
      }
      else
      {
        return assq(variable, second);
      }
    }

    inline auto identify(object const& variable,
                         object const& bound_variables)
    {
      assert(variable.is_also<identifier>());

      if (let const& identity = std::as_const(*this).identify(variable, bound_variables); identity != f)
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
                      object const& current_environment,
                      object const& binding_specs = unit) const -> pair
    {
      if (form.is<pair>() and car(form).is<pair>() and caar(form).is_also<identifier>())
      {
        if (let const& identity = identify(caar(form), bound_variables); identity.is<absolute>())
        {
          if (let const& value = cdr(identity); value.is<transformer>())
          {
            return sweep(cons(value.as<transformer>().transform(car(form), current_environment),
                              cdr(form)),
                         bound_variables,
                         current_environment,
                         binding_specs);
          }
          else if (value.is<syntax>())
          {
            if (auto const& name = value.as<syntax>().name; name == "begin")
            {
              return sweep(append(cdar(form), cdr(form)),
                           bound_variables,
                           current_environment,
                           binding_specs);
            }
            else if (name == "define") // <form> = ((define ...) <definition or expression>*)
            {
              if (let const& definition = car(form); cadr(definition).is<pair>()) // <form> = ((define (<variable> . <formals>) <body>) <definition or expression>*)
              {
                return sweep(cdr(form),
                             bound_variables,
                             current_environment,
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
                             current_environment,
                             cons(cdr(definition), binding_specs));
              }
            }
            else if (name == "define-syntax") // <form> = ((define-syntax <keyword> <transformer spec>) <definition or expression>*)
            {
              return sweep(cdr(form),
                           bound_variables,
                           current_environment,
                           cons(list(make<absolute>(cadar(form), // <keyword>
                                                    caddar(form))), // <transformer spec>
                                binding_specs));
            }
          }
        }
      }

      return pair(binding_specs, form);
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP
