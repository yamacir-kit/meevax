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

#ifndef INCLUDED_MEEVAX_KERNEL_STRING_HPP
#define INCLUDED_MEEVAX_KERNEL_STRING_HPP

#include <filesystem>
#include <vector>

#include <meevax/kernel/character.hpp>

namespace meevax::inline kernel
{
  struct string
  {
    std::vector<character> characters;

    explicit string(std::string const&);

    template <typename... Ts, typename = std::enable_if_t<std::is_constructible_v<std::vector<character>, Ts...>>>
    explicit string(Ts&&... xs)
      : characters { std::forward<decltype(xs)>(xs)... }
    {}

    auto utf8() const -> std::string;
  };

  auto make_string_from_list_of_character(let const&) -> object;

  auto operator ==(string const&, string const&) -> bool;
  auto operator !=(string const&, string const&) -> bool;
  auto operator < (string const&, string const&) -> bool;
  auto operator <=(string const&, string const&) -> bool;
  auto operator > (string const&, string const&) -> bool;
  auto operator >=(string const&, string const&) -> bool;

  auto operator <<(std::ostream &, string const&) -> std::ostream &;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
