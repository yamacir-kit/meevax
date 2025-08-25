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

#include <meevax/kernel/closure.hpp>
#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/syntactic_environment.hpp>
#include <meevax/kernel/transformer.hpp>

namespace meevax::inline kernel
{
  auto transformer::transform(let const& form, let const& use_environment) const -> object
  {
    /*
       Scheme programs can define and use new derived expression types,
       called macros. Program-defined expression types have the syntax

         (<keyword> <datum>...)

       where <keyword> is an identifier that uniquely determines the
       expression type. This identifier is called the syntactic keyword, or
       simply keyword, of the macro. The number of the <datum>s, and their
       syntax, depends on the expression type.

       Each instance of a macro is called a use of the macro. The set of
       rules that specifies how a use of a macro is transcribed into a more
       primitive expression is called the transformer of the macro.
    */
    assert(first.is<closure>());
    assert(second.is<syntactic_environment>());

    return environment().apply(first, form, use_environment, second);
  }

  auto operator <<(std::ostream & os, transformer const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << green("transformer ") << faint("#;", &datum) << magenta(")");
  }
} // namespace meevax::kernel
