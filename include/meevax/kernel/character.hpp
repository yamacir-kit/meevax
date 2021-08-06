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

#ifndef INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
#define INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP

#include <meevax/string/unicode.hpp>

namespace meevax
{
inline namespace kernel
{
  struct character
  {
    std::char_traits<char>::int_type codepoint;

    explicit character() = default;

    explicit character(std::char_traits<char>::int_type const); // integer->char

    explicit character(std::istream &); // read-char

    operator std::char_traits<char>::int_type() const; // char->integer

    operator codeunit() const;

    auto write(std::ostream &) const -> std::ostream &; // write-char
  };

  auto operator <<(std::ostream &, character const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
