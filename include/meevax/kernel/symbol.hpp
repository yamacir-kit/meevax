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

#ifndef INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
#define INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP

#include <meevax/kernel/identifier.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct symbol : public identifier
  {
    using value_type = std::string;

    value_type const value;

    template <typename... Ts>
    explicit symbol(Ts&&... xs)
      : value { std::forward<decltype(xs)>(xs)... }
    {}

    operator std::string() const noexcept
    {
      return value;
    }
  };

  auto operator <<(std::ostream &, symbol const&) -> std::ostream &;

  extern std::unordered_map<std::string, value_type> symbols;

  auto string_to_symbol(std::string const&) -> const_reference;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
