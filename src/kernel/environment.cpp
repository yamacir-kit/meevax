/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

namespace meevax
{
inline namespace kernel
{
  auto environment::evaluate(object const& expression) -> object try
  {
    auto is = [&](auto name)
    {
      return expression.is<pair>() and car(expression).is<symbol>() and car(expression).as<symbol>() == name;
    };

    if (is("define-library"))
    {
      meevax::define<library>(lexical_cast<std::string>(cadr(expression)), cddr(expression));
      return cadr(expression);
    }
    else if (is("import"))
    {
      for (let const& import_set : cdr(expression))
      {
        import(import_set);
      }

      return unspecified;
    }
    else if (is("include"))
    {
      for (let const& command_or_definition : include(cdr(expression), true))
      {
        evaluate(command_or_definition);
      }

      return unspecified;
    }
    else if (is("include-ci"))
    {
      for (let const& command_or_definition : include(cdr(expression), false))
      {
        evaluate(command_or_definition);
      }

      return unspecified;
    }
    else
    {
      /*
         In most cases, the s, e, c, and d registers are all null when evaluate
         is called. However, if environment::evaluate of the same environment
         is called during the execution of environment::evaluate, this is not
         the case, so it is necessary to save the register. For example,
         situations like evaluating
           (eval <expression> (interaction-environment))
         in the REPL.
      */
      if (s or e or c)
      {
        d = cons(std::exchange(s, unit),
                 std::exchange(e, unit),
                 std::exchange(c, unit), d);
      }

      let const result = execute(optimize(compile(expression)));

      if (d)
      {
        s = d[0];
        e = d[1];
        c = d[2];
        d = tail(d, 3);
      }

      return result;
    }
  }
  catch (object const& x)
  {
    if (x.is_also<error>())
    {
      x.as<error>().raise(); // NOTE: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=84476
      return unit;
    }
    else
    {
      throw error(make<string>("uncaught exception"), x);
    }
  }

  auto resolve(object const& form) -> object
  {
    if (form[0].as<symbol>() == "only") /* -------------------------------------
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
                          return is_truthy(memq(car(identity), identities));
                        },
                        resolve(import_set));
        };
      };

      return only(cadr(form))
                 (cddr(form));
    }
    else if (form[0].as<symbol>() == "except") /* ------------------------------
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
                          return not is_truthy(memq(car(identity), identities));
                        },
                        resolve(import_set));
        };
      };

      return except(cadr(form))
                   (cddr(form));
    }
    else if (form[0].as<symbol>() == "prefix") /* ------------------------------
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
    else if (form[0].as<symbol>() == "rename") /* ------------------------------
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
          return map([&](let const& identity)
                     {
                       assert(identity.is<absolute>());
                       assert(car(identity).is_also<identifier>());

                       if (let const& renaming = assq(car(identity), renamings); is_truthy(renaming))
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
    else if (auto iter = libraries().find(lexical_cast<std::string>(form)); iter != std::end(libraries()))
    {
      return std::get<1>(*iter).resolve();
    }
    else
    {
      throw error(make<string>("No such library"), form);
    }
  }

  auto environment::import(object const& import_set) -> void
  {
    for (let const& identity : resolve(import_set))
    {
      assert(identity.is<absolute>());

      if (not is_truthy(std::as_const(*this).identify(car(identity), unit, unit)) or interactive)
      {
        define(car(identity),
               cdr(identity));
      }
      else
      {
        throw error(make<string>("in a program or library declaration, it is an error to import the same identifier more than once with different bindings"), identity);
      }
    }
  }

  auto environment::load(std::string const& s) -> void
  {
    if (auto input = input_file_port(s); input.is_open() and input.get_ready())
    {
      while (not static_cast<std::istream &>(input).eof())
      {
        evaluate(input.read());
      }
    }
    else
    {
      throw file_error(make<string>("failed to open file"),
                       make<string>(s));
    }
  }

  auto operator <<(std::ostream & os, environment const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("environment ") << faint("#;", &datum) << magenta(")");
  }

  template struct configurator<environment>;

  template struct dynamic_environment<environment>;

  template struct syntactic_environment<environment>;
} // namespace kernel
} // namespace meevax
