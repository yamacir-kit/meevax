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

#include <meevax/kernel/import_set.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/library.hpp>

namespace meevax
{
inline namespace kernel
{
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
                          return is_truthy(memq(identity.as<absolute>().symbol(), identities));
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
                          return not is_truthy(memq(identity.as<absolute>().symbol(), identities));
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
                       return make<absolute>(make_symbol(car(prefixes).as<symbol>() + identity.as<absolute>().symbol().as<symbol>()),
                                             identity.as<absolute>().load());
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
                       if (let const& renaming = assq(identity.as<absolute>().symbol(), renamings); is_truthy(renaming))
                       {
                         assert(cadr(renaming).is<symbol>());
                         return make<absolute>(cadr(renaming), identity.as<absolute>().load());
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

  import_set::import_set(object const& form)
    : identities { resolve(form) }
  {}

  import_set::import_set(std::string const& library_name)
    : import_set { input_string_port(library_name).read() }
  {}

  auto import_set::operator ()(environment & e) const -> void
  {
    auto const redefinable = e.interactive or e == interaction_environment().as<environment>();

    for (let const& identity : identities)
    {
      assert(identity.is<absolute>());

      if (let const& variable = identity.as<absolute>().symbol(); eq(std::as_const(e).identify(variable, e.local(), unit), f) or redefinable)
      {
        e.define(identity.as<absolute>().symbol(), identity.as<absolute>().load());
      }
      else
      {
        throw error(make<string>("In a program or library declaration, it is an error to import the same identifier more than once with different bindings"), variable);
      }
    }
  }
} // namespace kernel
} // namespace meevax
