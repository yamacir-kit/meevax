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

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/expander.hpp>
#include <meevax/kernel/generator.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/syntactic_environment.hpp>
#include <meevax/kernel/syntax.hpp>
#include <meevax/kernel/transformer.hpp>

namespace meevax::inline kernel
{
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
    auto rename = syntactic_closure::renamer(&enclosure, nullptr, true);
    return expand(form, bound_variables, rename);
  }

  auto syntactic_environment::expand(object const& form,
                                     object const& bound_variables,
                                     syntactic_closure::renamer & rename) const -> object
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
                                    syntactic_closure::renamer & rename,
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
                                         cons(syntactic_environment::corename("lambda"),
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
