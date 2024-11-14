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

#ifndef INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
#define INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP

#include <meevax/kernel/identifier.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  struct symbol : public identifier
  {
    std::string const name;

    template <typename... Ts>
    explicit symbol(Ts&&... xs)
      : name { std::forward<decltype(xs)>(xs)... }
    {}

    auto operator <=>(symbol const&) const = default;

    operator std::string() const noexcept
    {
      return name;
    }
  };

  auto operator + (symbol const&, symbol const&) -> std::string;

  auto operator <<(std::ostream &, symbol const&) -> std::ostream &;

  template <typename T, typename = std::enable_if_t<is_equality_comparable_v<std::string const&, T const&>>>
  auto operator ==(symbol const& a, T const& b) -> bool
  {
    return a.name == b;
  }

  auto symbols() -> std::unordered_map<std::string, object> &;

  auto make_symbol(std::string const&) -> object const&;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYMBOL_HPP
