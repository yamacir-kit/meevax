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

#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/include.hpp>
#include <meevax/kernel/input_file_port.hpp>
#include <meevax/kernel/library.hpp>
#include <meevax/kernel/proper_list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax::inline kernel
{
  auto environment::evaluate(object const& expression) -> object
  {
    if (expression.is<pair>() and car(expression).is<symbol>())
    {
      if (auto&& name = car(expression).as<symbol>().name; name == "define-library")
      {
        libraries().emplace(lexical_cast(cadr(expression)), make<library>(cddr(expression)));

        return unspecified;
      }
      else if (name == "import")
      {
        for (let const& import_set : cdr(expression) | as_proper_list)
        {
          import(import_set);
        }

        return unspecified;
      }
      else if (name == "include")
      {
        for (let const& command_or_definition : include(cdr(expression)) | as_proper_list)
        {
          evaluate(command_or_definition);
        }

        return unspecified;
      }
      else if (name == "include-ci")
      {
        for (let const& command_or_definition : include<case_insensitive>(cdr(expression)) | as_proper_list)
        {
          evaluate(command_or_definition);
        }

        return unspecified;
      }
    }

    return execute(compile(expression));
  }

  auto import_set(object const& form) -> object
  {
    assert(form.is<pair>());

    if (car(form).as<symbol>().name == "only") /* ------------------------------
    *
    *  <declaration> = (only <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto only = [identifiers = cddr(form)](let const& identity)
      {
        assert(identity.is<absolute>());
        return memq(car(identity), identifiers) != f;
      };

      return filter(only, import_set(cadr(form)));
    }
    else if (car(form).as<symbol>().name == "except") /* -----------------------
    *
    *  <declaration> = (except <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto except = [identifiers = cddr(form)](let const& identity)
      {
        assert(identity.is<absolute>());
        return memq(car(identity), identifiers) == f;
      };

      return filter(except, import_set(cadr(form)));
    }
    else if (car(form).as<symbol>().name == "prefix") /* -----------------------
    *
    *  <declaration> = (prefix <import set> <identifier>)
    *
    * ----------------------------------------------------------------------- */
    {
      auto prefix = [prefix = caddr(form)](let const& identity)
      {
        assert(identity.is<absolute>());
        return make<absolute>(make_symbol(lexical_cast(prefix) +
                                          lexical_cast(car(identity))),
                              cdr(identity));
      };

      return map(prefix, import_set(cadr(form)));
    }
    else if (car(form).as<symbol>().name == "rename") /* -----------------------
    *
    *  <declaration> = (rename <import set>
    *                          (<identifier 1> <identifier 2>) ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto rename = [renamings = cddr(form)](let const& identity) -> object
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
      };

      return map(rename, import_set(cadr(form)));
    }
    else if (auto iter = libraries().find(lexical_cast(form)); iter != libraries().end())
    {
      return std::get<1>(*iter).as<library>().import_set();
    }
    else // SRFI 138
    {
      auto pathname = std::filesystem::path();

      for (let const& each : form | as_proper_list)
      {
        pathname /= lexical_cast(each);
      }

      environment().load(textual_context::of(form).locate(pathname, is_existing_non_directory));

      if (auto iterator = libraries().find(lexical_cast(form)); iterator != libraries().end())
      {
        return std::get<1>(*iterator).as<library>().import_set();
      }
      else
      {
        throw error(make<string>("No such library"), form);
      }
    }
  }

  auto environment::import(object const& form) -> void
  {
    for (let const& x : import_set(form) | as_proper_list)
    {
      assert(x.is<absolute>());

      if (let const& y = std::as_const(*this).identify(car(x), unit); y == f or this == std::addressof(interaction_environment().as<environment>()))
      {
        second = cons(x, second);
      }
      else if (x != y)
      {
        throw error(make<string>("in a program or library declaration, it is an error to import the same identifier more than once with different bindings"), x);
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
} // namespace meevax::kernel
