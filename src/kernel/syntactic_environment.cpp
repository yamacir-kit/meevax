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

#include <algorithm>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/conditional_expand.hpp>
#include <meevax/kernel/continuation.hpp>
#include <meevax/kernel/control.hpp>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/generator.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/include.hpp>
#include <meevax/kernel/proper_list.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/syntactic_environment.hpp>
#include <meevax/kernel/syntax.hpp>
#include <meevax/kernel/transformer.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax::inline kernel
{
  auto syntactic_environment::compile(object const& form) -> object
  {
    return generate(convert(expand(form, first), first), first);
  }

  auto syntactic_environment::convert(object const& form, object const& bound_variables) const -> object
  {
    return T_c(form, bound_variables, make<continuation>());
  }

  auto syntactic_environment::T_c(object const& form, object const& bound_variables, syntactic_continuation const& c) const -> object
  {
    auto T_star_k = [&](auto&&... xs)
    {
      auto recur = [&](auto recur, object const& form, administrative_beta_reducer const& k) -> object
      {
        if (form.is<pair>())
        {
          return T_k(car(form), bound_variables, [&](let const& x)
          {
            return recur(recur, cdr(form), [&](let const& xs)
            {
              return k(cons(x, xs));
            });
          });
        }
        else if (form.is<null>())
        {
          return k(unit);
        }
        else
        {
          return T_k(form, bound_variables, k);
        }
      };

      return recur(recur, std::forward<decltype(xs)>(xs)...);
    };

    if (not form.is<pair>())
    {
      return list(c, M(form, bound_variables));
    }
    else if (car(form).is_also<identifier>())
    {
      if (let const& identity = identify(car(form), bound_variables); identity.is_also<absolute>() and cdr(identity).is<syntax>())
      {
        switch (cdr(identity).as<syntax>().kind)
        {
        case syntax::quote:
        case syntax::quote_syntax:
        case syntax::current:
          return list(c, form);

        case syntax::lambda:
          return list(c, M(form, bound_variables));

        case syntax::conditional:
          if (c.is<pair>())
          {
            let const k = make<symbol>("$k");

            return list(list(default_rename("lambda"),
                             list(k),
                             T_c(form, bound_variables, k)),
                        c);
          }
          else
          {
            return T_k(cadr(form), bound_variables, [&](let const& a)
            {
              return list(default_rename("if"),
                          a,
                          T_c(caddr(form),                              bound_variables, c),
                          T_c(cdddr(form) ? cadddr(form) : unspecified, bound_variables, c));
            });
          }

        case syntax::set:
        case syntax::define:
        case syntax::install:
          return T_k(form, bound_variables, [&](let const& a)
          {
            return list(c, a);
          });

        case syntax::letrec:
          {
            let const formals1 = map(car, cadr(form));

            let const formals2 = map([](let const&) { return make<symbol>("$temporary"); }, cadr(form));

            let body = cddr(form);

            for (let xs = reverse(formals1), ys = reverse(formals2); xs; xs = cdr(xs), ys = cdr(ys))
            {
              body = cons(list(default_rename("set!"), car(xs), car(ys)), body);
            }

            return T_c(cons(list(default_rename("lambda"),
                                 formals1,
                                 cons(cons(default_rename("lambda"),
                                           formals2,
                                           body),
                                      map(cadr, cadr(form)))),
                            make_list(length(formals1), unspecified)),
                       bound_variables,
                       c);
          }

        case syntax::sequence:
          return converter::body(*this, cdr(form), bound_variables, c);

        case syntax::define_syntax:
          return list(default_rename("begin"), form, list(c, unspecified));

        case syntax::call_with_current_continuation:
          if (c.is<pair>())
          {
            let const k = make<symbol>("$k");

            return list(list(default_rename("lambda"),
                             list(k),
                             T_c(form, bound_variables, k)),
                        c);
          }
          else
          {
            return T_k(cadr(form), bound_variables, [&](let const& f)
                       {
                         let const xs = make<symbol>("$xs");

                         return list(f,
                                     c,
                                     list(default_rename("lambda"),
                                          cons(make<symbol>("$_"), xs),
                                          cons(c, xs)));
                       });
          }

        case syntax::call_with_values:
          return T_k(cadr(form), bound_variables, [&](let const& producer)
          {
            return T_k(caddr(form), bound_variables, [&](let const& consumer)
            {
              let const xs = make<symbol>("$xs");

              return list(producer,
                          list(default_rename("lambda"),
                               xs,
                               cons(consumer, c, xs)));
            });
          });

        default:
          assert(false);
          std::terminate();
        }
      }
    }

    return T_k(car(form), bound_variables, [&](let const& f)
    {
      return T_star_k(cdr(form), [&](let const& xs)
      {
        return cons(f, c, xs);
      });
    });
  }

  auto syntactic_environment::T_k(object const& form, object const& bound_variables, administrative_beta_reducer const& k) const -> object
  {
    if (not form.is<pair>())
    {
      return k(M(form, bound_variables));
    }
    else if (car(form).is_also<identifier>())
    {
      if (let const& identity = identify(car(form), bound_variables); identity.is_also<absolute>() and cdr(identity).is<syntax>())
      {
        return cdr(identity).as<syntax>().T_k(*this, form, bound_variables, k);
      }
    }

    return converter::call(*this, form, bound_variables, k);
  }

  auto syntactic_environment::M(object const& form, object const& bound_variables) const -> object
  {
    if (form.is<pair>())
    {
      let const k = make<symbol>("$k");

      return list(default_rename("lambda"),
                  cons(k, cadr(form)),
                  converter::body(*this,
                                  cddr(form),
                                  cons(cadr(form), bound_variables),
                                  k));
    }
    else
    {
      return form;
    }
  }

  auto syntactic_environment::define(object const& variable, object const& value) -> object
  {
    assert(variable.is_also<identifier>());
    assert(identify(variable, unit).template is<absolute>());
    cdr(identify(variable, unit)) = value;
    return variable;
  }

  auto syntactic_environment::expand(object const& form,
                                     object const& bound_variables) const -> object
  {
    auto enclosure = syntactic_closure(make<syntactic_environment>(bound_variables, second), unit, form);
    auto alpha = syntactic_closure::alpha(&enclosure, nullptr);
    return expand(form, bound_variables, alpha);
  }

  auto syntactic_environment::expand(object const& form,
                                     object const& bound_variables,
                                     syntactic_closure::alpha & alpha) const -> object
  {
    auto expand_operands = [&](auto&&... xs)
    {
      auto recur = [&](auto recur, object const& form) -> object
      {
        if (form.is<pair>())
        {
          return cons(expand(car(form), bound_variables, alpha), recur(recur, cdr(form)));
        }
        else
        {
          return expand(form, bound_variables, alpha);
        }
      };

      return recur(recur, std::forward<decltype(xs)>(xs)...);
    };

    auto expand_body = [this](auto&&... xs)
    {
      auto recur = [this](auto recur, object const& form, object const& bound_variables, auto & alpha)
      {
        if (auto [reversed_binding_specs,
                  sequence,
                  current_environment] = sweep(form,
                                               form,
                                               bound_variables,
                                               make<syntactic_environment>(bound_variables, second),
                                               alpha);
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

          for (let const& binding_spec : reversed_binding_specs | as_proper_list)
          {
            if (not car(binding_spec).is<macro>()) // The binding-spec is not an internal syntax definition.
            {
              sequence = cons(cons(default_rename("set!"), binding_spec), sequence);
            }
          }

          for (let & formal : formals | as_proper_list)
          {
            if (formal.is<macro>()) // is internal-sytnax-definition
            {
              cdr(formal) = make<transformer>(environment().execute(current_environment.template as<syntactic_environment>().compile(cdr(formal) /* <transformer spec> */)),
                                              current_environment);
            }
          }

          return expand(list(cons(cons(default_rename("lambda"),
                                       formals,
                                       sequence),
                                  make_list(length(formals), unit))),
                        bound_variables,
                        alpha);
        }
        else if (sequence.template is<pair>())
        {
          return cons(expand(car(sequence), bound_variables, alpha), recur(recur, cdr(sequence), bound_variables, alpha));
        }
        else
        {
          return expand(sequence, bound_variables, alpha);
        }
      };

      return recur(recur, std::forward<decltype(xs)>(xs)...);
    };

    auto expand_begin = [&](auto&&... xs)
    {
      auto recur = [&](auto recur, object const& form) -> object
      {
        if (form.is<pair>())
        {
          return cons(expand(car(form), bound_variables, alpha), recur(recur, cdr(form)));
        }
        else
        {
          return expand(form, bound_variables, alpha);
        }
      };

      return recur(recur, std::forward<decltype(xs)>(xs)...);
    };

    if (not form.is<pair>())
    {
      if (form.is<syntactic_closure>() and identify(form, bound_variables) == f)
      {
        return form.as<syntactic_closure>().expand(bound_variables, alpha);
      }
      else
      {
        return form.is_also<identifier>() ? alpha.convert(form) : form;
      }
    }
    else if (car(form).is_also<identifier>())
    {
      if (let const& identity = identify(alpha.convert(car(form)), bound_variables); identity.is_also<absolute>())
      {
        if (let const& value = cdr(identity); value.is<transformer>())
        {
          return expand(value.as<transformer>().transform(form, make<syntactic_environment>(bound_variables, second)),
                        bound_variables,
                        alpha);
        }
        else if (value.is<syntax>())
        {
          switch (value.as<syntax>().kind)
          {
          case syntax::quote:
          case syntax::quote_syntax:
            return form;

          case syntax::lambda:
            {
              auto scoped_alpha = alpha;

              let const& formals = scoped_alpha.convert_formals(cadr(form), bound_variables);

              return cons(alpha.convert(car(form)) /* lambda */,
                          cons(formals,
                               expand_body(cddr(form),
                                           cons(formals, bound_variables),
                                           scoped_alpha)));
            }

          case syntax::conditional:
          case syntax::set:
            return cons(alpha.convert(car(form)), expand_operands(cdr(form)));

          case syntax::include:
            return expand(cons(default_rename("begin"), include(cadr(form))), bound_variables, alpha);

          case syntax::include_case_insensitive:
            return expand(cons(default_rename("begin"), include<case_insensitive>(cadr(form))), bound_variables, alpha);

          case syntax::conditional_expand:
            return expand(cons(default_rename("begin"), conditional_expand(cdr(form))), bound_variables, alpha);

          case syntax::letrec:
            {
              let const extended_bound_variables = cons(alpha.convert_formals(map(car, cadr(form)), bound_variables), bound_variables);

              return cons(car(form),
                          map([&](let const& binding)
                              {
                                return list(car(binding), expand(cadr(binding), extended_bound_variables, alpha));
                              },
                              cadr(form)),
                          expand_body(cddr(form),
                                      extended_bound_variables,
                                      alpha));
            }

          case syntax::sequence:
            return expand_begin(form);

          case syntax::let_syntax:
            {
              let const current_environment = make<syntactic_environment>(bound_variables, second);

              auto formal = [&](let const& syntax_spec)
              {
                return make<macro>(car(syntax_spec) /* keyword */,
                                   make<transformer>(environment().execute(current_environment.as<syntactic_environment>().compile(cadr(syntax_spec) /* transformer spec */)),
                                                     current_environment));
              };

              let const formals = map(formal, cadr(form));

              return expand(list(cons(default_rename("lambda"),
                                      formals,
                                      cddr(form) /* body */)),
                            bound_variables,
                            alpha);
            }

          case syntax::letrec_syntax:
            {
              let current_environment = make<syntactic_environment>(bound_variables, second);

              auto formal = [&](let const& syntax_spec)
              {
                return make<macro>(car(syntax_spec) /* keyword */,
                                   make<transformer>(environment().execute(current_environment.as<syntactic_environment>().compile(cadr(syntax_spec) /* transformer spec */)),
                                                     current_environment));
              };

              let const formals = map(formal, cadr(form));

              current_environment.as<syntactic_environment>().first = cons(formals, bound_variables);

              return expand(list(cons(default_rename("lambda"),
                                      formals,
                                      cddr(form) /* body */)),
                            bound_variables,
                            alpha);
            }

          case syntax::define:
            {
              if (cadr(form).is<pair>()) // (define (<variable> . <formals>) <body>)
              {
                return list(alpha.convert(car(form)),
                            caadr(form) /* variable */,
                            expand(cons(default_rename("lambda"),
                                        cdadr(form) /* formals */,
                                        cddr(form) /* body */),
                                   bound_variables,
                                   alpha));
              }
              else // (define <variable> <expression>)
              {
                return cons(alpha.convert(car(form)),
                            cadr(form),
                            cddr(form) ? list(expand(caddr(form), bound_variables, alpha)) : unit);
              }
            }

          case syntax::define_syntax:
            return list(alpha.convert(car(form)), cadr(form), expand(caddr(form), bound_variables, alpha));

          case syntax::call_with_current_continuation:
          case syntax::call_with_values:
          case syntax::current:
          case syntax::install:
            return cons(alpha.convert(car(form)), expand_operands(cdr(form)));

          default:
            assert(false);
            std::terminate();
          }
        }
      }
    }

    return cons(expand(car(form), bound_variables, alpha), expand_operands(cdr(form)));
  }

  auto syntactic_environment::generate(object const& form,
                                       object const& bound_variables) -> object
  {
    let image = make<vector>();

    auto assemble = [&](auto assemble, object c, std::size_t offset) -> object
    {
      if (c.is<null>())
      {
        return c;
      }
      else switch (car(c).as<instruction>())
      {
      case instruction::call:
      case instruction::drop:
      case instruction::list_values:
        offset += 1;
        return cons(car(c), assemble(assemble, cdr(c), offset));

      case instruction::current:
      case instruction::install:
      case instruction::load_absolute:
      case instruction::load_constant:
      case instruction::load_relative:
      case instruction::load_variadic:
      case instruction::store_absolute:
      case instruction::store_relative:
      case instruction::store_variadic:
        offset += 2;
        return cons(car(c), cadr(c), assemble(assemble, cddr(c), offset));

      case instruction::load_closure:
        {
          offset += 2;

          auto const c1 = assemble(assemble, cddr(c), offset);
          auto const n1 = length(c1);

          return cons(car(c),
                      make<control>(image, make<small_integer>(static_cast<small_integer>(offset + n1))),
                      append(c1, assemble(assemble, cadr(c), offset + n1)));
        }

      case instruction::select:
        {
          offset += 3;

          auto const c2 = assemble(assemble, cdddr(c), offset);
          auto const n2 = length(c2);
          auto const c1 = assemble(assemble, cadr(c), offset + n2);
          auto const n1 = length(c1);

          return cons(car(c),
                      make<control>(image, make<small_integer>(static_cast<small_integer>(offset + n2))),
                      make<control>(image, make<small_integer>(static_cast<small_integer>(offset + n2 + n1))),
                      append(c2, append(c1, assemble(assemble, caddr(c), offset + n2 + n1))));
        }

      default:
        assert(false);
        return unit;
      }
    };

    std::ranges::copy(assemble(assemble, generate(form, bound_variables, unit), 0) | as_proper_list, std::back_inserter(image.as<vector>().objects));

    return image;
  }

  auto syntactic_environment::generate(object const& form,
                                       object const& bound_variables,
                                       object const& continuation) -> object
  {
    if (not form.is<pair>())
    {
      if (form.is_also<identifier>())
      {
        if (let const& identity = identify(form, bound_variables); identity.is<relative>())
        {
          return cons(make<instruction>(instruction::load_relative), identity, continuation);
        }
        else if (identity.is<variadic>())
        {
          return cons(make<instruction>(instruction::load_variadic), identity, continuation);
        }
        else
        {
          assert(identity.is_also<absolute>());
          return cons(make<instruction>(instruction::load_absolute), identity, continuation);
        }
      }
      else // is <self-evaluating>
      {
        return cons(make<instruction>(instruction::load_constant), form, continuation);
      }
    }
    else if (car(form).is_also<identifier>())
    {
      if (let const& identity = std::as_const(*this).identify(car(form), bound_variables); identity.is<absolute>() and cdr(identity).is<syntax>())
      {
        return cdr(identity).as<syntax>().generate(*this, cdr(form), bound_variables, continuation);
      }
    }

    return generator::call(*this, form, bound_variables, continuation);
  }

  auto syntactic_environment::identify(object const& variable,
                                       object const& bound_variables) const -> object
  {
    assert(variable.is_also<identifier>());

    for (auto i = 0; let formals : bound_variables | as_proper_list)
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
                                    syntactic_closure::alpha & alpha,
                                    let const& formals,
                                    let const& reversed_binding_specs) const -> std::tuple<object, object, object>
  {
    auto reset = [&](let const& formals)
    {
      return sweep(form,
                   form,
                   bound_variables,
                   make<syntactic_environment>(cons(formals, bound_variables), second),
                   alpha,
                   formals);
    };

    if (sequence.is<pair>())
    {
      if (car(sequence).is<syntactic_closure>() and identify(car(sequence), car(current_environment)) == f)
      {
        return sweep(form,
                     cons(car(sequence).as<syntactic_closure>().expand(car(current_environment), alpha),
                          cdr(sequence)),
                     bound_variables,
                     current_environment,
                     alpha,
                     formals,
                     reversed_binding_specs);
      }
      else if (car(sequence).is<pair>() and caar(sequence).is_also<identifier>())
      {
        if (let const& identity = identify(alpha.convert(caar(sequence)), bound_variables); identity.is_also<absolute>())
        {
          if (let const& value = cdr(identity); value.is<transformer>())
          {
            return sweep(form,
                         cons(value.as<transformer>().transform(car(sequence), current_environment),
                              cdr(sequence)),
                         bound_variables,
                         current_environment,
                         alpha,
                         formals,
                         reversed_binding_specs);
          }
          else if (value.is<syntax>())
          {
            if (auto const& name = value.as<syntax>().name.name; name == "begin")
            {
              return sweep(form,
                           append(cdar(sequence), cdr(sequence)),
                           bound_variables,
                           current_environment,
                           alpha,
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
                               alpha,
                               formals,
                               cons(list(variable,
                                         cons(default_rename("lambda"),
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
                               alpha,
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
              if (auto iter = std::ranges::find_if(formals | as_proper_list, [&](let const& formal)
                                                   {
                                                     return formal.is<macro>() and eq(car(formal), cadar(sequence));
                                                   });
                  iter != std::ranges::end(formals | as_proper_list))
              {
                return sweep(form,
                             cdr(sequence),
                             bound_variables,
                             current_environment,
                             alpha,
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

  auto core_syntactic_environment() -> object const&
  {
    #define BIND(NAME, SYNTAX) \
    make<absolute>(make_symbol(NAME), \
                   make<syntax>(NAME, \
                                syntax::SYNTAX, \
                                static_cast<decltype(syntax::T_k)>(converter::SYNTAX), \
                                generator::SYNTAX))

    let static const core_syntactic_environment = make<syntactic_environment>(
      unit,
      list(BIND("begin"                          , sequence                      ),
           BIND("call-with-current-continuation!", call_with_current_continuation),
           BIND("call-with-values!"              , call_with_values              ),
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

    return core_syntactic_environment;
  }

  auto default_rename(std::string const& variable) -> object
  {
    return make<syntactic_closure>(core_syntactic_environment(), unit, make_symbol(variable));
  }
} // namespace meevax::kernel
