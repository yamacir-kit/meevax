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

#include <meevax/kernel/conditional_expand.hpp>
#include <meevax/kernel/library.hpp>

namespace meevax::inline kernel
{
  auto conditional_expand_test(object const& requirement) -> bool
  {
    if (requirement.is<pair>() and car(requirement).is<symbol>())
    {
      if (car(requirement).as<symbol>() == "library")
      {
        return libraries().find(lexical_cast<std::string>(cadr(requirement))) != libraries().end();
      }
      else if (car(requirement).as<symbol>() == "and")
      {
        return std::all_of(cdr(requirement).begin(),
                           cdr(requirement).end(), conditional_expand_test);
      }
      else if (car(requirement).as<symbol>() == "or")
      {
        return std::any_of(cdr(requirement).begin(),
                           cdr(requirement).end(), conditional_expand_test);
      }
      else if (car(requirement).as<symbol>() == "not")
      {
        return not conditional_expand_test(cadr(requirement));
      }
      else
      {
        throw error(make<string>("unknown feature-request"), requirement);
      }
    }
    else
    {
      return requirement == make_symbol("else") or memq(requirement, features()) != f;
    }
  }

  auto conditional_expand(object const& clauses) -> object
  {
    for (let const& clause : clauses)
    {
      if (not clause.is<pair>())
      {
        throw error(make<string>("each clause of conditional-expand takes the form (<feature requirement> <expression> ...)"), clause);
      }
      else if (conditional_expand_test(car(clause)))
      {
        return cdr(clause);
      }
    }

    throw error(make<string>("no matching clauses were found"), clauses);
  }
} // namespace meevax::kernel
