/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#include <meevax/kernel/library.hpp>
#include <meevax/kernel/import_set.hpp>

namespace meevax
{
inline namespace kernel
{
  auto make_import_set(const_reference form) -> value_type
  {
    if (form[0].as<symbol>().value == "only") /* -------------------------------
    *
    *  <declaration> = (only <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto only = [](let const& import_set)
      {
        return [=](let const& identifiers)
        {
          return filter([&](let const& identity)
                        {
                          return select(memq(identity.as<absolute>().symbol(),
                                             identifiers));
                        },
                        make_import_set(import_set));
        };
      };

      return only(cadr(form))
                 (cddr(form));
    }
    else if (form[0].as<symbol>().value == "except") /* ------------------------
    *
    *  <declaration> = (except <import set> <identifier> ...)
    *
    * ----------------------------------------------------------------------- */
    {
      auto except = [](let const& import_set)
      {
        return [=](let const& identifiers)
        {
          return filter([&](let const& identity)
                        {
                          return not select(memq(identity.as<absolute>().symbol(),
                                                 identifiers));
                        },
                        make_import_set(import_set));
        };
      };

      return except(cadr(form))
                   (cddr(form));
    }
    else if (form[0].as<symbol>().value == "prefix") /* ------------------------
    *
    *  <declaration> = (prefix <import set> <identifier>)
    *
    * ----------------------------------------------------------------------- */
    {
      auto prefix = [](let const& import_set)
      {
        return [=](let const& prefixes)
        {
          return map1([&](let const& identity)
                      {
                        return make<absolute>(string_to_symbol(car(prefixes).as<symbol>().value +
                                                               identity.as<absolute>().symbol().as<symbol>().value),
                                              identity.as<absolute>().load());
                      },
                      make_import_set(import_set));
        };
      };

      return prefix(cadr(form))
                   (cddr(form));
    }
    else if (form[0].as<symbol>().value == "rename") /* ------------------------
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
          return map1([&](let const& identity)
                      {
                        if (let const& renaming = assq(identity.as<absolute>().symbol(),
                                                       renamings);
                            select(renaming))
                        {
                          assert(cadr(renaming).is<symbol>());
                          return make<absolute>(cadr(renaming), identity.as<absolute>().load());
                        }
                        else
                        {
                          return identity;
                        }
                      },
                      make_import_set(import_set));
        };
      };

      return rename(cadr(form))
                   (cddr(form));
    }
    else if (auto iter = libraries.find(lexical_cast<std::string>(form)); iter != std::end(libraries))
    {
      return std::get<1>(*iter).resolve();
    }
    else
    {
      throw error(make<string>("No such library"), form);
    }
  }

  import_set::import_set(const_reference form)
    : identifiers { make_import_set(form) }
  {}

  import_set::import_set(std::string const& library_name)
    : import_set { interaction_environment().as<environment>().read(library_name) }
  {}

  auto import_set::resolve(environment & e) const -> void
  {
    for (let const& identity : identifiers)
    {
      assert(identity.is<absolute>());

      if (let const& variable = identity.as<absolute>().symbol(); not eq(e[variable], undefined) and not e.interactive)
      {
        throw error(make<string>("In a program or library declaration, it is an error to import the same identifier more than once with different bindings"), variable);
      }
      else
      {
        e.define(identity.as<absolute>().symbol(), identity.as<absolute>().load());
      }
    }
  }
} // namespace kernel
} // namespace meevax
