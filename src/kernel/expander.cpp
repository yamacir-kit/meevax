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
#include <meevax/kernel/expander.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/include.hpp>
#include <meevax/kernel/transformer.hpp>

namespace meevax::inline kernel
{
  #define EXPANDER(NAME)                                                       \
  auto NAME([[maybe_unused]] syntactic_environment const& expander,            \
                             object const& form,                               \
            [[maybe_unused]] object const& bound_variables,                    \
            [[maybe_unused]] syntactic_closure::renamer & rename) -> object

  EXPANDER(expander::quote)
  {
    return form;
  }

  EXPANDER(expander::quote_syntax)
  {
    return form;
  }

  EXPANDER(expander::call)
  {
    return cons(expander.expand(car(form),
                                bound_variables,
                                rename),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(expander::operand)
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

  EXPANDER(expander::lambda)
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

  EXPANDER(expander::body)
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
          sequence = cons(cons(default_rename("set!"), binding_spec), sequence);
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

      return expander.expand(list(cons(cons(default_rename("lambda"),
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

  EXPANDER(expander::conditional)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(expander::set)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(expander::include)
  {
    return expander.expand(cons(default_rename("begin"),
                                meevax::include(cadr(form))),
                           bound_variables,
                           rename);
  }

  EXPANDER(expander::include_case_insensitive)
  {
    return expander.expand(cons(default_rename("begin"),
                                meevax::include(cadr(form), false)),
                           bound_variables,
                           rename);
  }

  EXPANDER(expander::conditional_expand)
  {
    return expander.expand(cons(default_rename("begin"),
                                meevax::conditional_expand(cdr(form))),
                           bound_variables,
                           rename);
  }

  EXPANDER(expander::letrec)
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

  EXPANDER(expander::sequence)
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

  EXPANDER(expander::let_syntax)
  {
    let const current_environment = make<syntactic_environment>(bound_variables, expander.second);

    auto formal = [&](let const& syntax_spec)
    {
      return make<macro>(car(syntax_spec) /* keyword */,
                         make<transformer>(environment().execute(current_environment.as<syntactic_environment>().compile(cadr(syntax_spec) /* transformer spec */)),
                                           current_environment));
    };

    let const formals = map(formal, cadr(form));

    return expander.expand(list(cons(default_rename("lambda"),
                                     formals,
                                     cddr(form) /* body */)),
                           bound_variables,
                           rename);
  }

  EXPANDER(expander::letrec_syntax)
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

    return expander.expand(list(cons(default_rename("lambda"),
                                     formals,
                                     cddr(form) /* body */)),
                           bound_variables,
                           rename);
  }

  EXPANDER(expander::define)
  {
    if (cadr(form).is<pair>()) // (define (<variable> . <formals>) <body>)
    {
      return list(rename(car(form)),
                  caadr(form) /* variable */,
                  expander.expand(cons(default_rename("lambda"),
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

  EXPANDER(expander::define_syntax)
  {
    return list(rename(car(form)),
                cadr(form),
                expander.expand(caddr(form),
                                bound_variables,
                                rename));
  }

  EXPANDER(expander::call_with_current_continuation)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(expander::current)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }

  EXPANDER(expander::install)
  {
    return cons(rename(car(form)),
                operand(expander,
                        cdr(form),
                        bound_variables,
                        rename));
  }
} // namespace meevax::kernel
