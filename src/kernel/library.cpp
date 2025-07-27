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

#include <meevax/kernel/library.hpp>

namespace meevax::inline kernel
{
  library::library(object const& declarations)
    : declarations { declarations }
  {}

  auto library::evaluate(object const& declaration) -> object
  {
    if (declaration.is<pair>() and car(declaration).is<symbol>())
    {
      if (auto&& name = car(declaration).as<symbol>().name; name == "export")
      {
        export_specs = append(cdr(declaration), export_specs);

        return unspecified;
      }
      else if (name == "begin")
      {
        for (let const& command_or_definition : cdr(declaration))
        {
          evaluator.evaluate(command_or_definition);
        }

        return unspecified;
      }
      else if (name == "include-library-declarations")
      {
        for (let const& library_declaration : include(cdr(declaration)))
        {
          evaluate(library_declaration);
        }

        return unspecified;
      }
      else if (name == "cond-expand")
      {
        for (let const& library_declaration : conditional_expand(cdr(declaration)))
        {
          evaluate(library_declaration);
        }

        return unspecified;
      }
    }

    return evaluator.evaluate(declaration); // Non-standard extension.
  }

  auto library::import_set() -> object
  {
    if (let const unresolved_declarations = std::exchange(declarations, unit); unresolved_declarations.is<pair>())
    {
      assert(declarations.is<null>());

      for (let const& unresolved_declaration : unresolved_declarations)
      {
        evaluate(unresolved_declaration);
      }
    }

    return map([this](let const& export_spec) -> object
               {
                 if (export_spec.is<pair>())
                 {
                   assert(car(export_spec).is<symbol>());
                   assert(car(export_spec).as<symbol>() == "rename");
                   assert(cadr(export_spec).is_also<identifier>());
                   assert(caddr(export_spec).is_also<identifier>());
                   return make<absolute>(caddr(export_spec),
                                         cdr(evaluator.identify(cadr(export_spec), unit)));
                 }
                 else
                 {
                   assert(export_spec.is_also<identifier>());
                   return evaluator.identify(export_spec, unit);
                 }
               },
               export_specs);
  }

  auto operator <<(std::ostream & os, library const& library) -> std::ostream &
  {
    return os << library.evaluator.second; // free-variables
  }

  auto libraries() -> std::map<std::string, object> &
  {
    static auto libraries = std::map<std::string, object>();
    return libraries;
  }
} // namespace meevax::kernel
