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

#include <unordered_map>

#include <meevax/kernel/miscellaneous.hpp>
#include <meevax/string/unicode.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- R7RS 6.6. Characters -------------------------------------------------
   *
   *  Characters are objects that represent printed characters such as letters
   *  and digits. All Scheme implementations must support at least the ASCII
   *  character repertoire: that is, Unicode characters U+0000 through U+007F.
   *  Implementations may support any other Unicode characters they see fit,
   *  and may also support non-Unicode characters as well. Except as otherwise
   *  specified, the result of applying any of the following procedures to a
   *  non-Unicode character is implementation-dependent.
   *
   * ------------------------------------------------------------------------ */
  struct character
  {
    codepoint value;

    explicit character() = default;

    /* ---- R7RS 6.6. Characters -----------------------------------------------
     *
     *  (char->integer char)                                          procedure
     *  (integer->char n)                                             procedure
     *
     *  Given a Unicode character, char->integer returns an exact integer
     *  between 0 and #xD7FF or between #xE000 and #x10FFFF which is equal to
     *  the Unicode scalar value of that character. Given a non-Unicode
     *  character, it returns an exact integer greater than #x10FFFF. This is
     *  true independent of whether the implementation uses the Unicode
     *  representation internally.
     *
     *  Given an exact integer that is the value returned by a character when
     *  char->integer is applied to it, integer->char returns that character.
     *
     *  NOTE: codepoint type is std::char_traits<char>::int_type
     *
     * ---------------------------------------------------------------------- */
    explicit constexpr character(codepoint const value)
      : value { value }
    {}

    /* ---- R7RS 6.13.2. Input -------------------------------------------------
     *
     *  (read-char)                                                   procedure
     *  (read-char port)                                              procedure
     *
     *  Returns the next character available from the textual input port,
     *  updating the port to point to the following character. If no more
     *  characters are available, an end-of-file object is returned.
     *
     * ---------------------------------------------------------------------- */
    explicit character(std::istream &);

    /* ---- R7RS 6.6. Characters -----------------------------------------------
     *
     *  (char->integer char)                                          procedure
     *  (integer->char n)                                             procedure
     *
     *  Given a Unicode character, char->integer returns an exact integer
     *  between 0 and #xD7FF or between #xE000 and #x10FFFF which is equal to
     *  the Unicode scalar value of that character. Given a non-Unicode
     *  character, it returns an exact integer greater than #x10FFFF. This is
     *  true independent of whether the implementation uses the Unicode
     *  representation internally.
     *
     *  Given an exact integer that is the value returned by a character when
     *  char->integer is applied to it, integer->char returns that character.
     *
     *  NOTE: codepoint type is std::char_traits<char>::int_type
     *
     * ---------------------------------------------------------------------- */
    constexpr operator codepoint() const
    {
      return value;
    }

    operator codeunit() const;

    auto read(std::istream &) const -> codepoint;

    [[deprecated]]
    auto read_codeunit(std::istream &) const -> codeunit;

    /* ---- R7RS 6.13.3. Output ------------------------------------------------
     *
     *  (write-char char)                                             procedure
     *  (write-char char port)                                        procedure
     *
     *  Writes the character char (not an external representation of the
     *  character) to the given textual output port and returns an unspecified
     *  value.
     *
     * --------------------------------------------------------------------- */
    auto write(std::ostream &) const -> std::ostream &;
  };

  /* ---- R7RS 6.6. Characters -------------------------------------------------
   *
   *  Characters are written using the notation #\<character> or
   *  #\<character name> or #\x<hex scalar value>.
   *
   *  The following character names must be supported by all implementations
   *  with the given values. Implementations may add other names provided they
   *  cannot be interpreted as hex scalar values preceded by x.
   *
   *    #\alarm     ; U+0007
   *    #\backspace ; U+0008
   *    #\delete    ; U+007F
   *    #\escape    ; U+001B
   *    #\newline   ; the linefeed character, U+000A
   *    #\null      ; the null character, U+0000
   *    #\return    ; the return character, U+000D
   *    #\space     ; the preferred way to write a space
   *    #\tab       ; the tab character, U+0009
   *
   *  Here are some additional examples:
   *
   *    #\a     ; lower case letter
   *    #\A     ; upper case letter
   *    #\(     ; left parenthesis
   *    #\      ; the space character
   *    #\x03BB ; λ (if character is supported)
   *    #\iota  ; ι (if character and name are supported)
   *
   *  Case is significant in #\<character>, and in #\<character name>, but not
   *  in #\x<hex scalar value>. If <character> in #\<character> is alphabetic,
   *  then any character immediately following <character> cannot be one that
   *  can appear in an identifier. This rule resolves the ambiguous case where,
   *  for example, the sequence of characters "#\space" could be taken to be
   *  either a representation of the space character or a representation of the
   *  character "#\s" followed by a representation of the symbol "pace."
   *
   *  Characters written in the #\ notation are self-evaluating. That is, they
   *  do not have to be quoted in programs.
   *
   * ------------------------------------------------------------------------ */
  auto operator <<(std::ostream &, character const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_CHARACTER_HPP
