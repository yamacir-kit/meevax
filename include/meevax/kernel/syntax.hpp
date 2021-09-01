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

#ifndef INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP

#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/syntactic_context.hpp>

#define SYNTAX(NAME)                                                           \
  auto NAME(                                                                   \
    [[maybe_unused]] syntactic_context const& current_syntactic_context,       \
    [[maybe_unused]] syntactic_continuation & current_syntactic_continuation,  \
    [[maybe_unused]] let const& expression,                                    \
    [[maybe_unused]] let const& frames,                                        \
    [[maybe_unused]] let const& continuation) -> pair::value_type

namespace meevax
{
inline namespace kernel
{
  class syntactic_continuation;

  struct syntax
    : public std::function<SYNTAX()>
  {
    using signature = SYNTAX((*));

    std::string const name;

    template <typename... Ts>
    explicit syntax(std::string const& name, Ts&&... xs)
      : std::function<SYNTAX()> { std::forward<decltype(xs)>(xs)...  }
      , name { name }
    {}

    template <typename... Ts>
    auto compile(Ts&&... xs) -> decltype(auto)
    {
      return std::invoke(*this, std::forward<decltype(xs)>(xs)...);
    }
  };

  auto operator <<(std::ostream &, syntax const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTAX_HPP
