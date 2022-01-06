/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP

#include <meevax/kernel/syntax.hpp>

namespace meevax
{
inline namespace kernel
{
  struct syntactic_continuation
  {
    context const preserved_context;

    std::reference_wrapper<environment> const preserved_environment;

    let const expression;

    let const frames;

    let const continuation;

    template <typename Compiler>
    auto apply(Compiler const& compile) -> decltype(auto)
    {
      return compile(preserved_context, preserved_environment, expression, frames, continuation);
    }
  };

  auto operator <<(std::ostream &, syntactic_continuation const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CONTINUATION_HPP
