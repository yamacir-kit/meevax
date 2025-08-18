/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <meevax/kernel/conditional_expand.hpp>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/include.hpp>
#include <meevax/kernel/syntactic_environment.hpp>
#include <meevax/kernel/transformer.hpp>

namespace meevax::inline kernel
{
  syntactic_environment::syntactic_closure::renamer::renamer(syntactic_closure const* enclosure, renamer * outer, bool transparent)
    : enclosure   { enclosure }
    , outer       { outer }
    , transparent { transparent }
  {
    assert(enclosure);
  }

  auto syntactic_environment::syntactic_closure::renamer::count(let const& form) -> int
  {
    assert(form.is<symbol>());

    return (outer ? outer->count(form) : 0) + std::count_if(dictionary.begin(), dictionary.end(), [&](let const& entry)
                                                            {
                                                              return eq(car(entry), form);
                                                            });
  }

  auto syntactic_environment::syntactic_closure::renamer::unshadow(let const& formals,
                                                                   let const& bound_variables) -> object
  {
    auto rename = [&](let const& form)
    {
      assert(form.is_also<identifier>() or form.is<macro>() or form.is<null>());

      if (form.is<symbol>() and std::any_of(bound_variables.begin(), bound_variables.end(), [&](let const& formals)
                                            {
                                              return meevax::memq(form, formals); // TODO variadic arguments
                                            }))
      {
        return make_syntactic_closure(form, count(form) + 1);
      }
      else
      {
        return form;
      }
    };

    if (formals.is<pair>())
    {
      return cons(rename(car(formals)), unshadow(cdr(formals), bound_variables));
    }
    else
    {
      return rename(formals);
    }
  }

  auto syntactic_environment::syntactic_closure::renamer::memq(let const& form) const -> object
  {
    if (let const& x = meevax::memq(form, enclosure->free_names); x != f)
    {
      return x;
    }
    else
    {
      return transparent and outer ? outer->memq(form) : f;
    }
  }

  auto syntactic_environment::syntactic_closure::renamer::assq(let const& form) const -> object
  {
    if (let const& x = meevax::assq(form, dictionary); x != f)
    {
      return x;
    }
    else
    {
      return transparent and outer ? outer->assq(form) : f;
    }
  }

  auto syntactic_environment::syntactic_closure::renamer::make_syntactic_closure(let const& form, int version) -> object const&
  {
    return cdar(dictionary = alist_cons(form,
                                        make<syntactic_closure>(enclosure->environment, unit, form, version),
                                        dictionary));
  }

  auto syntactic_environment::syntactic_closure::renamer::rename(let const& form) -> object
  {
    assert(form.is_also<identifier>() or form.is<macro>() or form.is<null>());

    auto inject = [this](let const& form)
    {
      return outer ? outer->rename(form) : form;
    };

    if (form.is<symbol>())
    {
      if (memq(form) != f)
      {
        return inject(form);
      }
      else if (let const& renaming = assq(form); renaming != f)
      {
        return cdr(renaming);
      }
      else
      {
        return transparent ? inject(form) : make_syntactic_closure(form);
      }
    }
    else
    {
      return form;
    }
  }

  auto syntactic_environment::syntactic_closure::renamer::operator ()(let const& form) -> object
  {
    return rename(form);
  }

  auto syntactic_environment::syntactic_closure::renamer::operator ()(let const& formals,
                                                                      let const& bound_variables) -> object
  {
    return unshadow(formals, bound_variables);
  }

  syntactic_environment::syntactic_closure::syntactic_closure(let const& environment,
                                                              let const& free_names,
                                                              let const& form,
                                                              int version)
    : environment { environment }
    , free_names  { free_names }
    , form        { form }
    , version     { version }
  {
    assert(environment.is<syntactic_environment>());
  }

  auto syntactic_environment::syntactic_closure::expand(let const& bound_variables, renamer & outer) -> object
  {
    auto rename = renamer(this,
                          &outer,
                          eq(environment.template as<syntactic_environment>().first,
                             bound_variables));

    return environment.as<syntactic_environment>().expand(form, bound_variables, rename);
  }

  auto syntactic_environment::syntactic_closure::identify(let const& bound_variables) -> object
  {
    auto identify = [&]()
    {
      let xs = environment.as<syntactic_environment>().first;

      for (auto offset = length(bound_variables) - length(xs); 0 < offset; --offset)
      {
        xs = cons(unit, xs);
      }

      return environment.as_const<syntactic_environment>().identify(form, xs);
    };

    if (let const& identity = identify(); identity != f)
    {
      return identity;
    }
    else
    {
      return environment.as_const<syntactic_environment>().identify(form, bound_variables);
    }
  }

  auto operator ==(syntactic_environment::syntactic_closure const& x,
                   syntactic_environment::syntactic_closure const& y) -> bool
  {
    /*
       (free-identifier=? id-1 id-2)                              procedure

       Returns #t if the original occurrences of id-1 and id-2 have the same
       binding, otherwise returns #f. free-identifier=? is used to look for a
       literal identifier in the argument to a transformer, such as else in a
       cond clause. A macro definition for syntax-rules would use
       free-identifier=? to look for literals in the input.
    */
    return x.form.template is_also<identifier>() and
           y.form.template is_also<identifier>() and
           eqv(x.environment.template as<syntactic_environment>().identify(x.form, x.environment.template as<syntactic_environment>().first),
               y.environment.template as<syntactic_environment>().identify(y.form, y.environment.template as<syntactic_environment>().first));
  }

  auto operator <<(std::ostream & os, syntactic_environment::syntactic_closure const& datum) -> std::ostream &
  {
    if (datum.form.template is_also<identifier>())
    {
      if (0 < datum.version)
      {
        return os << datum.form << ':' << datum.version;
      }
      else
      {
        return os << '$' << datum.form;
      }
    }
    else
    {
      return os << datum.form;
    }
  }

  #define EXPANDER(NAME)                                                       \
  auto NAME([[maybe_unused]] syntactic_environment const& expander,            \
                             object const& form,                               \
            [[maybe_unused]] object const& bound_variables,                    \
            [[maybe_unused]] typename syntactic_closure::renamer & rename) -> object

  EXPANDER(syntactic_environment::expander::quote)
  {
    return form;
  }

  EXPANDER(syntactic_environment::expander::quote_syntax)
  {
    return form;
  }

  EXPANDER(syntactic_environment::expander::call)
  {
    return cons(expander.expand(car(form),
                                bound_variables,
                                rename),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(syntactic_environment::expander::operand)
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

  EXPANDER(syntactic_environment::expander::lambda)
  {
    auto scoped_rename = rename;

    let const& formals = scoped_rename(cadr(form), bound_variables);

    return cons(rename(car(form)) /* lambda */,
                cons(formals,
                     body(expander,
                          cddr(form),
                          cons(formals, bound_variables),
                          scoped_rename)));
  }

  EXPANDER(syntactic_environment::expander::body)
  {
    if (auto [reversed_binding_specs,
              sequence,
              current_environment] = expander.sweep(form,
                                                    form,
                                                    bound_variables,
                                                    make<syntactic_environment>(bound_variables, expander.second),
                                                    rename);
        reversed_binding_specs)
    {
      /*
         (letrec* <binding specs> <sequence>)

             => ((lambda <variables> <assignments> <sequence>)
                 <dummy 1> ... <dummy n>)

         where <binding specs> = ((<variable 1> <initial 1>) ...
                                  (<variable n> <initial n>))
      */
      let & formals = caar(current_environment);

      for (let const& binding_spec : reversed_binding_specs)
      {
        if (not car(binding_spec).is<macro>()) // The binding-spec is not an internal syntax definition.
        {
          sequence = cons(cons(corename("set!"), binding_spec), sequence);
        }
      }

      for (let & formal : formals)
      {
        if (formal.is<macro>()) // is internal-sytnax-definition
        {
          cdr(formal) = make<transformer>(environment().execute(current_environment.template as<syntactic_environment>().compile(cdr(formal) /* <transformer spec> */)),
                                          current_environment);
        }
      }

      return expander.expand(list(cons(cons(corename("lambda"),
                                            formals,
                                            sequence),
                                       make_list(length(formals), unit))),
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

  EXPANDER(syntactic_environment::expander::conditional)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(syntactic_environment::expander::set)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(syntactic_environment::expander::include)
  {
    return expander.expand(cons(corename("begin"),
                                meevax::include(cadr(form))),
                           bound_variables,
                           rename);
  }

  EXPANDER(syntactic_environment::expander::include_case_insensitive)
  {
    return expander.expand(cons(corename("begin"),
                                meevax::include(cadr(form), false)),
                           bound_variables,
                           rename);
  }

  EXPANDER(syntactic_environment::expander::conditional_expand)
  {
    return expander.expand(cons(corename("begin"),
                                meevax::conditional_expand(cdr(form))),
                           bound_variables,
                           rename);
  }

  EXPANDER(syntactic_environment::expander::letrec)
  {
    let const extended_bound_variables = cons(rename(map(car, cadr(form)), bound_variables),
                                              bound_variables);

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

  EXPANDER(syntactic_environment::expander::sequence)
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

  EXPANDER(syntactic_environment::expander::let_syntax)
  {
    let const current_environment = make<syntactic_environment>(bound_variables, expander.second);

    auto formal = [&](let const& syntax_spec)
    {
      return make<macro>(car(syntax_spec) /* keyword */,
                         make<transformer>(environment().execute(current_environment.as<syntactic_environment>().compile(cadr(syntax_spec) /* transformer spec */)),
                                           current_environment));
    };

    let const formals = map(formal, cadr(form));

    return expander.expand(list(cons(corename("lambda"),
                                     formals,
                                     cddr(form) /* body */)),
                           bound_variables,
                           rename);
  }

  EXPANDER(syntactic_environment::expander::letrec_syntax)
  {
    let current_environment = make<syntactic_environment>(bound_variables, expander.second);

    auto formal = [&](let const& syntax_spec)
    {
      return make<macro>(car(syntax_spec) /* keyword */,
                         make<transformer>(environment().execute(current_environment.as<syntactic_environment>().compile(cadr(syntax_spec) /* transformer spec */)),
                                           current_environment));
    };

    let const formals = map(formal, cadr(form));

    current_environment.as<syntactic_environment>().first = cons(formals, bound_variables);

    return expander.expand(list(cons(corename("lambda"),
                                     formals,
                                     cddr(form) /* body */)),
                           bound_variables,
                           rename);
  }

  EXPANDER(syntactic_environment::expander::define)
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

  EXPANDER(syntactic_environment::expander::define_syntax)
  {
    return list(rename(car(form)),
                cadr(form),
                expander.expand(caddr(form),
                                bound_variables,
                                rename));
  }

  EXPANDER(syntactic_environment::expander::call_with_current_continuation)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(syntactic_environment::expander::current)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(syntactic_environment::expander::install)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  #define GENERATOR(NAME)                                                      \
  auto NAME([[maybe_unused]] syntactic_environment & generator,                \
            [[maybe_unused]] object const& form,                               \
            [[maybe_unused]] object const& bound_variables,                    \
            [[maybe_unused]] object const& continuation,                       \
            [[maybe_unused]] bool tail) -> object

  GENERATOR(syntactic_environment::generator::quote)
  {
    return cons(make(instruction::secd_load_constant), car(form).is<syntactic_closure>() ? car(form).as<syntactic_closure>().form
                                                                                         : car(form),
                continuation);
  }

  GENERATOR(syntactic_environment::generator::quote_syntax)
  {
    return cons(make(instruction::secd_load_constant), car(form),
                continuation);
  }

  GENERATOR(syntactic_environment::generator::call)
  {
    return operand(generator,
                   cdr(form),
                   bound_variables,
                   generator.generate(car(form),
                                      bound_variables,
                                      tail ? list(make(instruction::secd_tail_call))
                                           : cons(make(instruction::secd_call), continuation)));
  }

  GENERATOR(syntactic_environment::generator::operand)
  {
    if (form.is<pair>())
    {
      return operand(generator,
                     cdr(form),
                     bound_variables,
                     generator.generate(car(form),
                                        bound_variables,
                                        cons(make(instruction::secd_cons),
                                             continuation)));
    }
    else
    {
      return generator.generate(form, bound_variables, continuation);
    }
  }

  GENERATOR(syntactic_environment::generator::lambda)
  {
    return cons(make(instruction::secd_load_closure),
                body(generator,
                     cdr(form),
                     cons(car(form), bound_variables), // Extend scope.
                     list(make(instruction::secd_return))),
                continuation);
  }

  GENERATOR(syntactic_environment::generator::body)
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
                                cons(make(instruction::secd_drop),
                                     body(generator,
                                          cdr(form),
                                          bound_variables,
                                          continuation)),
                                false);
    }
  }

  GENERATOR(syntactic_environment::generator::conditional)
  {
    if (tail)
    {
      assert(lexical_cast(continuation) == "(return)");

      return generator.generate(car(form), // <test>
                                bound_variables,
                                list(make(instruction::secd_tail_select),
                                     generator.generate(cadr(form),
                                                        bound_variables,
                                                        continuation,
                                                        tail),
                                     cddr(form) ? generator.generate(caddr(form),
                                                                     bound_variables,
                                                                     continuation,
                                                                     tail)
                                                : list(make(instruction::secd_load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                       make(instruction::secd_return))));
    }
    else
    {
      return generator.generate(car(form), // <test>
                                bound_variables,
                                cons(make(instruction::secd_select),
                                     generator.generate(cadr(form),
                                                        bound_variables,
                                                        list(make(instruction::secd_join))),
                                     cddr(form) ? generator.generate(caddr(form),
                                                                     bound_variables,
                                                                     list(make(instruction::secd_join)))
                                                : list(make(instruction::secd_load_constant), unspecified, // If <test> yields a false value and no <alternate> is specified, then the result of the expression is unspecified.
                                                       make(instruction::secd_join)),
                                     continuation));
    }
  }

  GENERATOR(syntactic_environment::generator::set)
  {
    assert(car(form).is_also<identifier>());

    if (let const& identity = generator.identify(car(form), bound_variables); identity.is<relative>())
    {
      return generator.generate(cadr(form),
                                bound_variables,
                                cons(make(instruction::secd_store_relative), identity,
                                     continuation));
    }
    else if (identity.is<variadic>())
    {
      return generator.generate(cadr(form),
                                bound_variables,
                                cons(make(instruction::secd_store_variadic), identity,
                                     continuation));
    }
    else
    {
      assert(identity.is<absolute>());

      return generator.generate(cadr(form),
                                bound_variables,
                                cons(make(instruction::secd_store_absolute), identity,
                                     continuation));
    }
  }

  GENERATOR(syntactic_environment::generator::letrec)
  {
    assert(not tail or lexical_cast(continuation) == "(return)");

    let const formals = map(car, car(form));

    return cons(make(instruction::secd_dummy),
                operand(generator,
                        map(cadr, car(form)),
                        cons(formals, bound_variables),
                        lambda(generator,
                               cons(formals, cdr(form)), // (<formals> <body>)
                               bound_variables,
                               tail ? list(make(instruction::secd_tail_letrec))
                                    : cons(make(instruction::secd_letrec), continuation))));
  }

  GENERATOR(syntactic_environment::generator::sequence)
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
                    cons(make(instruction::secd_drop), // Pop result of head expression
                         sequence(generator,
                                  cdr(form), // Rest expression or definitions
                                  bound_variables,
                                  continuation,
                                  tail)));
    }
  }

  GENERATOR(syntactic_environment::generator::define)
  {
    assert(not car(form).is<pair>()); // This has been checked on previous passes.

    assert(car(form).is_also<identifier>());

    if (bound_variables)
    {
      throw error(make<string>("definition cannot appear in this syntactic-context"));
    }
    else
    {
      return generator.generate(cdr(form) ? cadr(form) : unspecified,
                                bound_variables,
                                cons(make(instruction::secd_store_absolute), generator.identify(car(form), bound_variables),
                                     continuation));
    }
  }

  GENERATOR(syntactic_environment::generator::define_syntax)
  {
    assert(car(form).is_also<identifier>());

    let identity = generator.identify(car(form), unit);

    cdr(identity) = make<transformer>(environment().execute(generator.generate(cadr(form),
                                                                               bound_variables)),
                                      make<syntactic_environment>(bound_variables,
                                                                  generator.second));

    return cons(make(instruction::secd_load_constant), unspecified,
                continuation);
  }

  GENERATOR(syntactic_environment::generator::call_with_current_continuation)
  {
    assert(form.is<pair>());
    assert(cdr(form).is<null>());

    return cons(make(instruction::secd_load_continuation),
                continuation,
                generator.generate(car(form),
                                   bound_variables,
                                   list(make(instruction::secd_tail_call)), // The first argument passed to call-with-current-continuation must be called via a tail call.
                                   tail));
  }

  GENERATOR(syntactic_environment::generator::current)
  {
    return cons(make(instruction::secd_current), car(form),
                continuation);
  }

  GENERATOR(syntactic_environment::generator::install)
  {
    return generator.generate(cadr(form),
                              bound_variables,
                              cons(make(instruction::secd_install), car(form),
                                   continuation));
  }

  auto syntactic_environment::compile(object const& form) -> object
  {
    return generate(expand(form, first), first);
  }

  auto syntactic_environment::core() -> object const&
  {
    #define BIND(NAME, SYNTAX) make<absolute>(make_symbol(NAME), make<syntax>(NAME, expander::SYNTAX, generator::SYNTAX))

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

  auto syntactic_environment::corename(std::string const& variable) -> object
  {
    return make<syntactic_closure>(core(), unit, make_symbol(variable));
  }

  auto syntactic_environment::define(object const& variable,
                                     object const& value) -> void
  {
    assert(variable.is_also<identifier>());
    assert(identify(variable, unit).template is<absolute>());
    cdr(identify(variable, unit)) = value;
  }

  auto syntactic_environment::expand(object const& form,
                                     object const& bound_variables) const -> object
  {
    auto enclosure = syntactic_closure(make<syntactic_environment>(bound_variables, second), unit, form);
    auto rename = typename syntactic_closure::renamer(&enclosure, nullptr, true);
    return expand(form, bound_variables, rename);
  }

  auto syntactic_environment::expand(object const& form,
                                     object const& bound_variables,
                                     typename syntactic_closure::renamer & rename) const -> object
  {
    if (not form.is<pair>())
    {
      if (form.is<syntactic_closure>() and identify(form, bound_variables) == f)
      {
        return form.as<syntactic_closure>().expand(bound_variables, rename);
      }

      return form.is_also<identifier>() ? rename(form) : form;
    }
    else if (car(form).is_also<identifier>())
    {
      let const identifier = rename(car(form));

      if (let const& identity = identifier.is<syntactic_closure>() ? identifier.as<syntactic_closure>().identify(bound_variables)
                                                                   : identify(car(form), bound_variables);
          identity.is_also<absolute>())
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

  auto syntactic_environment::generate(object const& form,
                                       object const& bound_variables,
                                       object const& continuation,
                                       bool tail) -> object
  {
    if (not form.is<pair>())
    {
      if (form.is_also<identifier>())
      {
        assert(form.is_also<identifier>());

        if (let const& identity = identify(form, bound_variables); identity.is<relative>())
        {
          return cons(make(instruction::secd_load_relative), identity, continuation);
        }
        else if (identity.is<variadic>())
        {
          return cons(make(instruction::secd_load_variadic), identity, continuation);
        }
        else
        {
          assert(identity.is_also<absolute>());
          return cons(make(instruction::secd_load_absolute), identity, continuation);
        }
      }
      else // is <self-evaluating>
      {
        return cons(make(instruction::secd_load_constant), form, continuation);
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

  auto syntactic_environment::identify(object const& variable,
                                       object const& bound_variables) const -> object
  {
    assert(variable.is_also<identifier>());

    for (auto i = 0; let formals : bound_variables)
    {
      for (auto j = 0; not formals.is<null>(); formals = cdr(formals))
      {
        if (formals.is<pair>())
        {
          if (car(formals).is<macro>() and eq(caar(formals), variable))
          {
            return car(formals);
          }
          else if (eq(car(formals), variable))
          {
            return make<relative>(make<small_integer>(i), make<small_integer>(j));
          }
        }
        else if (formals.is_also<identifier>() and eq(formals, variable))
        {
          return make<variadic>(make<small_integer>(i), make<small_integer>(j));
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

  auto syntactic_environment::identify(object const& variable,
                                       object const& bound_variables) -> object
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
         is not bound, or is a syntactic keyword, then the definition will bind
         <variable> to a new location before performing the assignment, whereas
         it would be an error to perform a set! on an unbound variable.
      */
      assert(not variable.is<syntactic_closure>());

      return car(second = cons(make<absolute>(variable, undefined), second));
    }
  }

  auto syntactic_environment::sweep(let const& form,
                                    let const& sequence,
                                    let const& bound_variables,
                                    let const& current_environment,
                                    typename syntactic_closure::renamer & rename,
                                    let const& formals,
                                    let const& reversed_binding_specs) const -> std::tuple<object, object, object>
  {
    auto reset = [&](let const& formals)
    {
      return sweep(form,
                   form,
                   bound_variables,
                   make<syntactic_environment>(cons(formals, bound_variables), second),
                   rename,
                   formals);
    };

    if (sequence.is<pair>())
    {
      if (car(sequence).is<syntactic_closure>() and identify(car(sequence), car(current_environment)) == f)
      {
        return sweep(form,
                     cons(car(sequence).as<syntactic_closure>().expand(car(current_environment), rename),
                          cdr(sequence)),
                     bound_variables,
                     current_environment,
                     rename,
                     formals,
                     reversed_binding_specs);
      }
      else if (car(sequence).is<pair>() and caar(sequence).is_also<identifier>())
      {
        if (let const& identity = identify(caar(sequence), bound_variables); identity.is_also<absolute>())
        {
          if (let const& value = cdr(identity); value.is<transformer>())
          {
            return sweep(form,
                         cons(value.as<transformer>().transform(car(sequence), current_environment),
                              cdr(sequence)),
                         bound_variables,
                         current_environment,
                         rename,
                         formals,
                         reversed_binding_specs);
          }
          else if (value.is<syntax>())
          {
            if (auto const& name = value.as<syntax>().name; name == "begin")
            {
              return sweep(form,
                           append(cdar(sequence), cdr(sequence)),
                           bound_variables,
                           current_environment,
                           rename,
                           formals,
                           reversed_binding_specs);
            }
            else if (name == "define") // <form> = ((define ...) <definition or expression>*)
            {
              if (cadar(sequence).is<pair>()) // <form> = ((define (<variable> . <formals>) <body>) <definition or expression>*)
              {
                if (let const& variable = caadar(sequence); memq(variable, formals) != f)
                {
                  return sweep(form,
                               cdr(sequence),
                               bound_variables,
                               current_environment,
                               rename,
                               formals,
                               cons(list(variable,
                                         cons(corename("lambda"),
                                              cdadar(sequence), // <formals>
                                              cddar(sequence))),
                                    reversed_binding_specs));
                }
                else
                {
                  return reset(append(formals, list(variable)));
                }
              }
              else // <form> = ((define <variable> <expression>) <definition or expression>*)
              {
                if (let const& variable = cadar(sequence); memq(variable, formals) != f)
                {
                  return sweep(form,
                               cdr(sequence),
                               bound_variables,
                               current_environment,
                               rename,
                               formals,
                               cons(cdar(sequence), // (<variables> <expression>)
                                    reversed_binding_specs));
                }
                else
                {
                  return reset(append(formals, list(variable)));
                }
              }
            }
            else if (name == "define-syntax") // <form> = ((define-syntax <keyword> <transformer spec>) <definition or expression>*)
            {
              if (auto iter = std::find_if(formals.begin(), formals.end(), [&](let const& formal)
                                           {
                                             return formal.is<macro>() and eq(car(formal), cadar(sequence));
                                           });
                  iter != formals.end())
              {
                return sweep(form,
                             cdr(sequence),
                             bound_variables,
                             current_environment,
                             rename,
                             formals,
                             cons(list(*iter), // <transformer spec>
                                  reversed_binding_specs));
              }
              else
              {
                return reset(append(formals, list(make<macro>(cadar(sequence), caddar(sequence)))));
              }
            }
          }
        }
      }
    }

    return std::make_tuple(reversed_binding_specs, sequence, current_environment);
  }
} // namespace meevax::kernel
