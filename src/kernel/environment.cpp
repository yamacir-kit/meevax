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

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/input_file_port.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/optimizer.hpp>

namespace meevax::inline kernel
{
  auto environment::evaluate(object const& expression) -> object
  {
    if (expression.is<pair>() and car(expression).is<symbol>())
    {
      if (auto&& name = car(expression).as<symbol>().name; name == "define-library")
      {
        meevax::define<library>(lexical_cast<std::string>(cadr(expression)), cddr(expression));

        return unspecified;
      }
      else if (name == "import")
      {
        for (let const& import_set : cdr(expression))
        {
          import(import_set);
        }

        return unspecified;
      }
      else if (name == "include")
      {
        for (let const& command_or_definition : include(cdr(expression), true))
        {
          evaluate(command_or_definition);
        }

        return unspecified;
      }
      else if (name == "include-ci")
      {
        for (let const& command_or_definition : include(cdr(expression), false))
        {
          evaluate(command_or_definition);
        }

        return unspecified;
      }
    }

    /*
       In most cases, the s, e, c, and d registers are all null when evaluate
       is called. However, if environment::evaluate of the same environment is
       called during the execution of environment::evaluate, this is not the
       case, so it is necessary to save the registers. For example, situations
       like evaluating

         (eval <expression> (interaction-environment))

       in the REPL.
    */
    struct dump
    {
      environment * context;

      let s, e, c, d;

      explicit dump(environment * context)
        : context { context }
        , s { std::exchange(context->s, nullptr) }
        , e { std::exchange(context->e, nullptr) }
        , c { std::exchange(context->c, nullptr) }
        , d { std::exchange(context->d, nullptr) }
      {}

      ~dump()
      {
        undump();
      }

      auto operator ()() -> void
      {
        undump();
      }

      auto undump() -> void
      {
        context->s = s;
        context->e = e;
        context->c = c;
        context->d = d;
      }
    };

    auto undump = dump(this);

    return execute(optimize(compile(expression)));
  }

  auto resolve(object const& form) -> object
  {
    assert(form.is<pair>());

    if (car(form).as<symbol>() == "only") /* -----------------------------------
    *
    *  <declaration> = (only <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto only = [](let const& import_set)
      {
        return [=](let const& identities)
        {
          return filter([&](let const& identity)
                        {
                          assert(identity.is<absolute>());
                          return memq(car(identity), identities) != f;
                        },
                        resolve(import_set));
        };
      };

      return only(cadr(form))
                 (cddr(form));
    }
    else if (car(form).as<symbol>() == "except") /* ----------------------------
    *
    *  <declaration> = (except <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto except = [](let const& import_set)
      {
        return [=](let const& identities)
        {
          return filter([&](let const& identity)
                        {
                          assert(identity.is<absolute>());
                          return memq(car(identity), identities) == f;
                        },
                        resolve(import_set));
        };
      };

      return except(cadr(form))
                   (cddr(form));
    }
    else if (car(form).as<symbol>() == "prefix") /* ----------------------------
    *
    *  <declaration> = (prefix <import set> <identifier>)
    *
    * ----------------------------------------------------------------------- */
    {
      auto prefix = [](let const& import_set)
      {
        return [=](let const& prefixes)
        {
          return map([&](let const& identity)
                     {
                       assert(identity.is<absolute>());
                       return make<absolute>(make_symbol(lexical_cast<std::string>(car(prefixes)) + lexical_cast<std::string>(car(identity))),
                                             cdr(identity));
                     },
                     resolve(import_set));
        };
      };

      return prefix(cadr(form))
                   (cddr(form));
    }
    else if (car(form).as<symbol>() == "rename") /* ----------------------------
    *
    *  <declaration> = (rename <import set>
    *                          (<identifier 1> <identifier 2>) ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto rename = [](let const& import_set)
      {
        return [=](let const& renamings)
        {
          return map([&](let const& identity) -> object
                     {
                       assert(identity.is<absolute>());
                       assert(car(identity).is_also<identifier>());

                       if (let const& renaming = assq(car(identity), renamings); renaming != f)
                       {
                         assert(cadr(renaming).is<symbol>());
                         return make<absolute>(cadr(renaming), cdr(identity));
                       }
                       else
                       {
                         return identity;
                       }
                     },
                     resolve(import_set));
        };
      };

      return rename(cadr(form))
                   (cddr(form));
    }
    else if (auto iter = libraries().find(lexical_cast<std::string>(form)); iter != libraries().end())
    {
      return std::get<1>(*iter).as<library>().resolve();
    }
    else
    {
      throw error(make<string>("No such library"), form);
    }
  }

  auto environment::import(object const& import_set) -> void
  {
    for (let const& immigrant : resolve(import_set))
    {
      assert(immigrant.is<absolute>());

      if (let const& inhabitant = std::as_const(*this).identify(car(immigrant), unit); inhabitant == f or interactive)
      {
        second = cons(immigrant, second);
      }
      else if (immigrant != inhabitant)
      {
        throw error(make<string>("in a program or library declaration, it is an error to import the same identifier more than once with different bindings"), immigrant);
      }
    }
  }

  auto environment::load(std::filesystem::path const& p) -> void
  {
    if (auto input = input_file_port(p); input.is_open())
    {
      for (let const& x : input)
      {
        evaluate(x);
      }
    }
    else
    {
      throw file_error(make<string>("failed to open file"),
                       make<string>(p));
    }
  }

  auto operator <<(std::ostream & os, environment const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("environment ") << faint("#;", &datum) << magenta(")");
  }

  template struct configurator<environment>;

  template struct syntactic_environment<environment>;
} // namespace meevax::kernel
