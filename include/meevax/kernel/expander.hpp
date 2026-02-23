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

#ifndef INCLUDED_MEEVAX_KERNEL_EXPANDER_HPP
#define INCLUDED_MEEVAX_KERNEL_EXPANDER_HPP

#include <meevax/kernel/syntactic_environment.hpp>

namespace meevax::inline kernel
{
  struct expander
  {
    #define EXPANDER(NAME)                                                   \
    auto NAME([[maybe_unused]] syntactic_environment const& expander,        \
                               object const& form,                           \
              [[maybe_unused]] object const& bound_variables,                \
              [[maybe_unused]] syntactic_closure::renamer & rename) -> object

    static EXPANDER(quote);

    static EXPANDER(quote_syntax);

    static EXPANDER(call);

    static EXPANDER(operand);

    static EXPANDER(lambda);

    static EXPANDER(body);

    static EXPANDER(conditional);

    static EXPANDER(set);

    static EXPANDER(include);

    static EXPANDER(include_case_insensitive);

    static EXPANDER(conditional_expand);

    static EXPANDER(letrec);

    static EXPANDER(sequence);

    static EXPANDER(let_syntax);

    static EXPANDER(letrec_syntax);

    static EXPANDER(define);

    static EXPANDER(define_syntax);

    static EXPANDER(call_with_current_continuation);

    static EXPANDER(current);

    static EXPANDER(install);

    #undef EXPANDER
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_EXPANDER_HPP
