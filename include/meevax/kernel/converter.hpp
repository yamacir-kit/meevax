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

#ifndef INCLUDED_MEEVAX_KERNEL_CONVERTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CONVERTER_HPP

#include <meevax/kernel/object.hpp>

namespace meevax::inline kernel
{
  struct syntactic_environment;

  using syntactic_continuation = object;

  using administrative_beta_reducer = std::function<auto (object const&) -> object>;

  struct converter // Matt Might's one-pass CPS transformation: https://matt.might.net/articles/cps-conversion/
  {
    #define CONVERTER(NAME) \
    static auto NAME(syntactic_environment const&, object const&, object const&, syntactic_continuation      const& c) -> object; \
    static auto NAME(syntactic_environment const&, object const&, object const&, administrative_beta_reducer const& k) -> object

    CONVERTER(quote);

    CONVERTER(quote_syntax);

    CONVERTER(call);

    CONVERTER(lambda);

    CONVERTER(body);

    CONVERTER(conditional);

    CONVERTER(set);

    static constexpr auto include = nullptr;

    static constexpr auto include_case_insensitive = nullptr;

    static constexpr auto conditional_expand = nullptr;

    CONVERTER(letrec);

    CONVERTER(sequence);

    static constexpr auto let_syntax = nullptr;

    static constexpr auto letrec_syntax = nullptr;

    CONVERTER(define);

    CONVERTER(define_syntax);

    CONVERTER(call_with_current_continuation);

    CONVERTER(call_with_values);

    CONVERTER(current);

    CONVERTER(install);

    #undef CONVERTER
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_CONVERTER_HPP
