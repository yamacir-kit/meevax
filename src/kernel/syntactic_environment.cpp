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

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/syntactic_environment.hpp>

namespace meevax::inline kernel
{
  auto syntactic_environment::transformer::transform(let const& form, let const& environment) const -> object
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

    return meevax::environment().apply(first, form, environment, second);
  }

  auto syntactic_environment::expander::body([[maybe_unused]] syntactic_environment const& expander,
                                                              object const& form,
                                             [[maybe_unused]] object const& bound_variables,
                                             [[maybe_unused]] typename syntactic_closure::renamer & rename) -> object
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

  auto syntactic_environment::expander::let_syntax([[maybe_unused]] syntactic_environment const& expander,
                                                                    object const& form,
                                                   [[maybe_unused]] object const& bound_variables,
                                                   [[maybe_unused]] typename syntactic_closure::renamer & rename) -> object
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

  auto syntactic_environment::expander::letrec_syntax([[maybe_unused]] syntactic_environment const& expander,
                                                                       object const& form,
                                                      [[maybe_unused]] object const& bound_variables,
                                                      [[maybe_unused]] typename syntactic_closure::renamer & rename) -> object
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

  auto syntactic_environment::generator::define_syntax([[maybe_unused]] syntactic_environment & generator,
                                                       [[maybe_unused]] object const& form,
                                                       [[maybe_unused]] object const& bound_variables,
                                                       [[maybe_unused]] object const& continuation,
                                                       [[maybe_unused]] bool tail) -> object
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
} // namespace meevax::kernel
