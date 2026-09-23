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

#include <meevax/kernel/converter.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/syntactic_environment.hpp>

namespace meevax::inline kernel
{
  #define CONVERTER(NAME, ...) \
  auto NAME([[maybe_unused]] syntactic_environment const& converter, \
                             object const& form, \
            [[maybe_unused]] object const& bound_variables, \
                             __VA_ARGS__) -> object

  CONVERTER(converter::quote, syntactic_continuation const& c)
  {
    return list(c, form);
  }

  CONVERTER(converter::quote, administrative_beta_reducer const& k)
  {
    return k(form);
  }

  CONVERTER(converter::quote_syntax, syntactic_continuation const& c)
  {
    return list(c, form);
  }

  CONVERTER(converter::quote_syntax, administrative_beta_reducer const& k)
  {
    return k(form);
  }

  CONVERTER(T_star_k, administrative_beta_reducer const& k)
  {
    if (form.is<pair>())
    {
      return converter.T_k(car(form), bound_variables, [&](let const& x)
      {
        return T_star_k(converter, cdr(form), bound_variables, [&](let const& xs)
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
      return converter.T_k(form, bound_variables, k);
    }
  }

  CONVERTER(converter::call, syntactic_continuation const& c)
  {
    return converter.T_k(car(form), bound_variables, [&](let const& f)
    {
      return T_star_k(converter, cdr(form), bound_variables, [&](let const& xs)
      {
        return cons(f, c, xs);
      });
    });
  }

  CONVERTER(converter::call, administrative_beta_reducer const& k)
  {
    let const x = make<symbol>("$x");

    return call(converter, form, bound_variables, list(default_rename("lambda"), list(x), k(x)));
  }

  CONVERTER(converter::lambda, syntactic_continuation const& c)
  {
    return list(c, converter.M(form, bound_variables));
  }

  CONVERTER(converter::lambda, administrative_beta_reducer const& k)
  {
    return k(converter.M(form, bound_variables));
  }

  CONVERTER(converter::body, syntactic_continuation const& c)
  {
    if (form.is<null>())
    {
      return list(c, unspecified);
    }
    else if (cdr(form).is<null>())
    {
      return converter.T_c(car(form), bound_variables, c);
    }
    else
    {
      return converter.T_c(car(form),
                           bound_variables,
                           list(default_rename("lambda"),
                                make<symbol>("$xs"),
                                body(converter,
                                     cdr(form),
                                     bound_variables,
                                     c)));
    }
  }

  CONVERTER(converter::body, administrative_beta_reducer const& k)
  {
    if (form.is<null>())
    {
      return k(unspecified);
    }
    else if (cdr(form).is<null>())
    {
      return converter.T_k(car(form), bound_variables, k);
    }
    else
    {
      return converter.T_c(car(form),
                           bound_variables,
                           list(default_rename("lambda"),
                                make<symbol>("$xs"),
                                body(converter,
                                     cdr(form),
                                     bound_variables,
                                     k)));
    }
  }

  CONVERTER(converter::conditional, syntactic_continuation const& c)
  {
    if (c.is<pair>())
    {
      let const k = make<symbol>("$k");

      return list(list(default_rename("lambda"),
                       list(k),
                       conditional(converter, form, bound_variables, k)),
                  c);
    }
    else
    {
      return converter.T_k(cadr(form), bound_variables, [&](let const& a)
      {
        return list(default_rename("if"),
                    a,
                    converter.T_c(caddr(form),                              bound_variables, c),
                    converter.T_c(cdddr(form) ? cadddr(form) : unspecified, bound_variables, c));
      });
    }
  }

  CONVERTER(converter::conditional, administrative_beta_reducer const& k)
  {
    let const x = make<symbol>("$x");

    return conditional(converter, form, bound_variables, list(default_rename("lambda"), list(x), k(x)));
  }

  CONVERTER(converter::set, syntactic_continuation const& c)
  {
    return set(converter, form, bound_variables, [&](let const& a)
    {
      return list(c, a);
    });
  }

  CONVERTER(converter::set, administrative_beta_reducer const& k)
  {
    return converter.T_k(cddr(form) ? caddr(form) : unspecified, bound_variables, [&](let const& a)
    {
      return list(default_rename("begin"),
                  list(car(form),
                       converter.M(cadr(form), bound_variables),
                       a),
                  k(unspecified));
    });
  }

  CONVERTER(converter::letrec, syntactic_continuation const& c)
  {
    let const formals1 = map(car, cadr(form));

    let const formals2 = map([](let const&) { return make<symbol>("$temporary"); }, cadr(form));

    let body = cddr(form);

    for (let xs = reverse(formals1), ys = reverse(formals2); xs; xs = cdr(xs), ys = cdr(ys))
    {
      body = cons(list(default_rename("set!"), car(xs), car(ys)), body);
    }

    return converter.T_c(cons(list(default_rename("lambda"),
                                   formals1,
                                   cons(cons(default_rename("lambda"),
                                             formals2,
                                             body),
                                        map(cadr, cadr(form)))),
                              make_list(length(formals1), unspecified)),
                         bound_variables,
                         c);
  }

  CONVERTER(converter::letrec, administrative_beta_reducer const& k)
  {
    let const x = make<symbol>("$x");

    return letrec(converter, form, bound_variables, list(default_rename("lambda"), list(x), k(x)));
  }

  CONVERTER(converter::sequence, syntactic_continuation const& c)
  {
    return body(converter, cdr(form), bound_variables, c);
  }

  CONVERTER(converter::sequence, administrative_beta_reducer const& k)
  {
    return body(converter, cdr(form), bound_variables, k);
  }

  CONVERTER(converter::define, syntactic_continuation const& c)
  {
    return define(converter, form, bound_variables, [&](let const& a)
    {
      return list(c, a);
    });
  }

  CONVERTER(converter::define, administrative_beta_reducer const& k)
  {
    assert(bound_variables.is<null>());
    return set(converter, cons(default_rename("set!"), cdr(form)), bound_variables, k);
  }

  CONVERTER(converter::define_syntax, syntactic_continuation const& c)
  {
    return list(default_rename("begin"), form, list(c, unspecified));
  }

  CONVERTER(converter::define_syntax, administrative_beta_reducer const& k)
  {
    return list(default_rename("begin"), form, k(unspecified));
  }

  CONVERTER(converter::call_with_current_continuation, syntactic_continuation const& c)
  {
    if (c.is<pair>())
    {
      let const k = make<symbol>("$k");

      return list(list(default_rename("lambda"),
                       list(k),
                       call_with_current_continuation(converter, form, bound_variables, k)),
                  c);
    }
    else
    {
      return converter.T_k(cadr(form), bound_variables, [&](let const& f)
      {
        let const xs = make<symbol>("$xs");

        return list(f,
                    c,
                    list(default_rename("lambda"),
                         cons(make<symbol>("$_"), xs),
                         cons(c, xs)));
      });
    }
  }

  CONVERTER(converter::call_with_current_continuation, administrative_beta_reducer const& k)
  {
    let const x = make<symbol>("$x");

    return call_with_current_continuation(converter, form, bound_variables, list(default_rename("lambda"), list(x), k(x)));
  }

  CONVERTER(converter::call_with_values, syntactic_continuation const& c)
  {
    return converter.T_k(cadr(form), bound_variables, [&](let const& producer)
    {
      return converter.T_k(caddr(form), bound_variables, [&](let const& consumer)
      {
        let const xs = make<symbol>("$xs");

        return list(producer,
                    list(default_rename("lambda"),
                         xs,
                         cons(consumer, c, xs)));
      });
    });
  }

  CONVERTER(converter::call_with_values, administrative_beta_reducer const& k)
  {
    let const x = make<symbol>("$x");

    return call_with_values(converter, form, bound_variables, list(default_rename("lambda"), list(x), k(x)));
  }

  CONVERTER(converter::current, syntactic_continuation const& c)
  {
    return list(c, form);
  }

  CONVERTER(converter::current, administrative_beta_reducer const& k)
  {
    return k(form);
  }

  CONVERTER(converter::install, syntactic_continuation const& c)
  {
    return set(converter, form, bound_variables, c);
  }

  CONVERTER(converter::install, administrative_beta_reducer const& k)
  {
    return set(converter, form, bound_variables, k);
  }

  #undef CONVERTER
} // namespace meevax::kernel
