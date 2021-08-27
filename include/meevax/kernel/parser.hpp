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

#ifndef INCLUDED_MEEVAX_KERNEL_PARSER_HPP
#define INCLUDED_MEEVAX_KERNEL_PARSER_HPP

#include <string>

namespace meevax
{
inline namespace kernel
{
  auto char_eq = [](auto c, auto... cs) constexpr
  {
    return (std::char_traits<decltype(c)>::eq(c, cs) or ...);
  };

  /* ---- Whitespace -----------------------------------------------------------
   *
   *  <intraline whitespace> = <space or tab>
   *
   *  <line ending> = <newline> | <return> <newline> | <return>
   *
   *  <whitespace> = <intraline whitespace> | <line ending>
   *
   * ------------------------------------------------------------------------ */

  auto is_intraline_whitespace = [](auto c) constexpr
  {
    return char_eq(c, ' ', '\f', '\t', '\v');
  };

  auto is_end_of_line = [](auto c) constexpr
  {
    return char_eq(c, '\n', '\r');
  };

  auto is_eof = [](auto c) constexpr
  {
    return char_eq(c, std::char_traits<decltype(c)>::eof());
  };

  auto is_whitespace = [](auto c) constexpr
  {
    return is_intraline_whitespace(c) or is_end_of_line(c) or is_eof(c);
  };

  /* ---- Delimiter ------------------------------------------------------------
   *
   *  <vertial line> = |
   *
   *  <delimiter> = <whitespace> | <vertial line> | ( | ) | " | ;
   *
   * ------------------------------------------------------------------------ */

  auto is_vertical_line = [](auto c) constexpr
  {
    return char_eq(c, '|');
  };

  auto is_delimiter = [](auto c) constexpr
  {
    return is_whitespace(c)
        or is_vertical_line(c)
        or char_eq(c, '(', ')', '[', ']', '{', '}', '"', ';');
  };

  /* ---- Token ---------------------------------------------------
   *
   *  <token> = <identifier> | <boolean> | <number> | <character> | <string>
   *          | ( | ) | #( | #u8 | ' | ` | , | ,@ | .
   *
   * ------------------------------------------------------------------------ */

  auto is_reader_macro_introducer = [](auto c) constexpr
  {
    return char_eq(c, '#', '\'', '`', ',');
  };

  auto is_end_of_token [[deprecated]] = [](auto c) constexpr
  {
    return is_delimiter(c) or is_reader_macro_introducer(c);
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PARSER_HPP
