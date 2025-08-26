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

#ifndef INCLUDED_MEEVAX_KERNEL_GENERATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_GENERATOR_HPP

#include <meevax/kernel/syntactic_environment.hpp>

namespace meevax::inline kernel
{
  struct generator
  {
    #define GENERATOR(NAME)                                                  \
    auto NAME([[maybe_unused]] syntactic_environment & generator,            \
              [[maybe_unused]] object const& form,                           \
              [[maybe_unused]] object const& bound_variables,                \
              [[maybe_unused]] object const& continuation,                   \
              [[maybe_unused]] bool tail = false) -> object

    static GENERATOR(quote);

    static GENERATOR(quote_syntax);

    static GENERATOR(call);

    static GENERATOR(operand);

    static GENERATOR(lambda);

    static GENERATOR(body);

    static GENERATOR(conditional);

    static GENERATOR(set);

    static constexpr auto include = nullptr;

    static constexpr auto include_case_insensitive = nullptr;

    static constexpr auto conditional_expand = nullptr;

    static GENERATOR(letrec);

    static GENERATOR(sequence);

    static constexpr auto let_syntax = nullptr;

    static constexpr auto letrec_syntax = nullptr;

    static GENERATOR(define);

    static GENERATOR(define_syntax);

    static GENERATOR(call_with_current_continuation);

    static GENERATOR(current);

    static GENERATOR(install);

    #undef GENERATOR
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_GENERATOR_HPP
