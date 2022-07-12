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

#ifndef INCLUDED_MEEVAX_KERNEL_STRING_HPP
#define INCLUDED_MEEVAX_KERNEL_STRING_HPP

#include <meevax/kernel/character.hpp>

namespace meevax
{
inline namespace kernel
{
  auto cat = [](auto&&... xs)
  {
    std::stringstream ss;
    (ss << ... << xs);
    return ss.str();
  };

  struct string
  {
    std::vector<character> codepoints;

    explicit string() = default;

    explicit string(std::istream &, std::size_t = std::numeric_limits<std::size_t>::max()); // read-string

    explicit string(std::istream &&);

    explicit string(external_representation const&);

    template <typename... Ts>
    explicit string(decltype(cat), Ts&&... xs)
      : string { cat(std::forward<decltype(xs)>(xs)...) }
    {}

    explicit string(std::size_t const k, character const& c)
      : codepoints { k, c }
    {}

    auto copy(const_reference, const_reference) const -> value_type;

    auto length() const -> value_type;

    auto list(std::size_t, std::size_t) const -> meevax::value_type;

    auto list(std::size_t = 0) const -> meevax::value_type;

    operator external_representation() const; // write-string (for display)
  };

  auto operator ==(string const&, string const&) -> bool;

  auto operator <<(std::ostream &, string const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
