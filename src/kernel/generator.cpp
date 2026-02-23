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
#include <meevax/kernel/generator.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/transformer.hpp>

namespace meevax::inline kernel
{
  #define GENERATOR(NAME)                                                      \
  auto NAME([[maybe_unused]] syntactic_environment & generator,                \
            [[maybe_unused]] object const& form,                               \
            [[maybe_unused]] object const& bound_variables,                    \
            [[maybe_unused]] object const& continuation,                       \
            [[maybe_unused]] bool tail) -> object

  GENERATOR(generator::quote)
  {
    return cons(make(instruction::secd_load_constant), car(form).is<syntactic_closure>() ? car(form).as<syntactic_closure>().form
                                                                                         : car(form),
                continuation);
  }

  GENERATOR(generator::quote_syntax)
  {
    return cons(make(instruction::secd_load_constant), car(form),
                continuation);
  }

  GENERATOR(generator::call)
  {
    return operand(generator,
                   cdr(form),
                   bound_variables,
                   generator.generate(car(form),
                                      bound_variables,
                                      tail ? list(make(instruction::secd_tail_call))
                                           : cons(make(instruction::secd_call), continuation)));
  }

  GENERATOR(generator::operand)
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

  GENERATOR(generator::lambda)
  {
    return cons(make(instruction::secd_load_closure),
                body(generator,
                     cdr(form),
                     cons(car(form), bound_variables), // Extend scope.
                     list(make(instruction::secd_return))),
                continuation);
  }

  GENERATOR(generator::body)
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

  GENERATOR(generator::conditional)
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

  GENERATOR(generator::set)
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

  GENERATOR(generator::letrec)
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

  GENERATOR(generator::sequence)
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

  GENERATOR(generator::define)
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

  GENERATOR(generator::define_syntax)
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

  GENERATOR(generator::call_with_current_continuation)
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

  GENERATOR(generator::current)
  {
    return cons(make(instruction::secd_current), car(form),
                continuation);
  }

  GENERATOR(generator::install)
  {
    return generator.generate(cadr(form),
                              bound_variables,
                              cons(make(instruction::secd_install), car(form),
                                   continuation));
  }
} // namespace meevax::kernel
