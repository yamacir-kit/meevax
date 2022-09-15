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

#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct character
  {
    using char_type = char;

    using int_type = std::char_traits<char_type>::int_type;

    int_type codepoint;

    explicit character() = default;

    explicit constexpr character(int_type const& codepoint)
      : codepoint { codepoint }
    {}

    static constexpr auto eq(int_type const& c1, int_type const& c2)
    {
      return std::char_traits<char_type>::eq_int_type(c1, c2);
    }

    inline constexpr auto eq(int_type const& c) const
    {
      return std::char_traits<char_type>::eq_int_type(codepoint, c);
    }

    static constexpr auto is_eof(int_type const& c)
    {
      return eq(std::char_traits<char_type>::eof(), c);
    }

    inline constexpr operator int_type() const
    {
      return codepoint;
    }

    explicit operator std::string() const; // write-char (for display)
  };

  auto operator <<(std::ostream &, character const&) -> std::ostream &; // write
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
