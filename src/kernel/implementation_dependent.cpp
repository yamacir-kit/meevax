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

#include <meevax/kernel/implementation_dependent.hpp>
#include <meevax/kernel/library.hpp>

namespace meevax
{
inline namespace kernel
{
  auto test(object const& requirement) -> bool
  {
    if (requirement.is<pair>() and car(requirement).is<symbol>())
    {
      if (car(requirement).as<symbol>() == "library")
      {
        return libraries().find(lexical_cast<std::string>(cadr(requirement))) != std::end(libraries());
      }
      else if (car(requirement).as<symbol>() == "and")
      {
        return std::all_of(std::begin(cdr(requirement)), std::end(cdr(requirement)), test);
      }
      else if (car(requirement).as<symbol>() == "or")
      {
        return std::any_of(std::begin(cdr(requirement)), std::end(cdr(requirement)), test);
      }
      else if (car(requirement).as<symbol>() == "not")
      {
        return not test(cadr(requirement));
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

  auto implementation_dependent(object const& clauses) -> object
  {
    for (let const& clause : clauses)
    {
      if (not clause.is<pair>())
      {
        throw error(make<string>("each clause of implementation-dependent takes the form (<feature requirement> <expression> ...)"), clause);
      }
      else if (test(car(clause)))
      {
        return cdr(clause);
      }
    }

    throw error(make<string>("no matching clauses were found"), clauses);
  }
} // namespace kernel
} // namespace meevax
