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
    return char_eq(c, u8' ', u8'\f', u8'\t', u8'\v');
  };

  auto is_end_of_line = [](auto c) constexpr
  {
    return char_eq(c, u8'\n', u8'\r');
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
    return char_eq(c, u8'|');
  };

  auto is_delimiter = [](auto c) constexpr
  {
    return is_whitespace(c)
        or is_vertical_line(c)
        or char_eq(c, u8'(', u8')', u8'[', u8']', u8'{', u8'}', u8'"', u8';');
  };

  /* ---- Token ---------------------------------------------------
   *
   *  <token> = <identifier> | <boolean> | <number> | <character> | <string>
   *          | ( | ) | #( | #u8 | ' | ` | , | ,@ | .
   *
   * ------------------------------------------------------------------------ */

  auto is_reader_macro_introducer = [](auto c) constexpr
  {
    return char_eq(c, u8'#', u8'\'', u8'`', u8',');
  };

  auto is_end_of_token = [](auto c) constexpr
  {
    return is_delimiter(c) or is_reader_macro_introducer(c);
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PARSER_HPP
