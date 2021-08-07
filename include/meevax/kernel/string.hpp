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

#ifndef INCLUDED_MEEVAX_KERNEL_STRING_HPP
#define INCLUDED_MEEVAX_KERNEL_STRING_HPP

#include <meevax/kernel/character.hpp>
#include <meevax/string/unicode.hpp> // DEPRECATED

namespace meevax
{
inline namespace kernel
{
  using characters = std::vector<character>;

  /* ---- R7RS 6.7. Strings ----------------------------------------------------
   *
   *  Strings are sequences of characters. Strings are written as sequences of
   *  characters enclosed within quotation marks ("). Within a string literal,
   *  various escape sequences represent characters other than themselves.
   *  Escape sequences always start with a backslash (\):
   *
   *    \a : alarm, U+0007
   *    \b : backspace, U+0008
   *    \t : character tabulation, U+0009
   *    \n : linefeed, U+000A
   *    \r : return, U+000D
   *    \" : double quote, U+0022
   *    \\ : backslash, U+005C
   *    \| : vertical line, U+007C
   *    \<intraline whitespace>* <line ending> <intraline whitespace>* : nothing
   *    \x<hex scalar value>; : specified character (note the terminating semi-colon).
   *
   *  The result is unspecified if any other character in a string occurs after
   *  a backslash.  Except for a line ending, any character outside of an
   *  escape sequence stands for itself in the string literal. A line ending
   *  which is preceded by \<intraline whitespace> expands to nothing (along
   *  with any trailing intraline whitespace), and can be used to indent
   *  strings for improved legibility. Any other line ending has the same
   *  effect as inserting a \n character into the string.
   *
   *  Examples:
   *
   *    "The word \"recursion\" has many meanings."
   *    "Another example:\ntwo lines of text"
   *    "Here's text \
   *       containing just one line"
   *    "\x03B1; is named GREEK SMALL LETTER ALPHA."
   *
   *  The length of a string is the number of characters that it contains.
   *  This number is an exact, non-negative integer that is fixed when the
   *  string is created. The valid indexes of a string are the exact
   *  non-negative integers less than the length of the string. The first
   *  character of a string has index 0, the second has index 1, and so on.
   *
   *  Some of the procedures that operate on strings ignore the difference
   *  between upper and lower case. The names of the versions that ignore case
   *  end with "-ci" (for "case insensitive").
   *
   *  Implementations may forbid certain characters from ap- pearing in
   *  strings. However, with the exception of #\null, ASCII characters must not
   *  be forbidden. For example, an implementation might support the entire
   *  Unicode repertoire, but only allow characters U+0001 to U+00FF (the
   *  Latin-1 repertoire without #\null) in strings.
   *
   *  It is an error to pass such a forbidden character to make-string, string,
   *  string-set!, or string-fill!, as part of the list passed to list->string,
   *  or as part of the vector passed to vector->string (see section 6.8), or
   *  in UTF-8 encoded form within a bytevector passed to utf8->string (see
   *  section 6.9). It is also an error for a procedure passed to string-map
   *  (see section 6.10) to return a forbidden character, or for read-string
   *  (see section 6.13.2) to attempt to read one.
   *
   * ------------------------------------------------------------------------ */
  struct string : public characters // TODO PRIVATE u32vector
  {
    explicit string() = default;

    explicit string(std::istream &);

    explicit string(std::string const&);

    explicit string(size_type, character const&);

    template <typename InputIterator>
    explicit string(InputIterator begin, InputIterator end)
      : characters { begin, end }
    {}

    /* ---- R7RS 6.13.2. Input -------------------------------------------------
     *
     *  (read-string k)                                               procedure
     *  (read-string k port)                                          procedure
     *
     *  Reads the next k characters, or as many as are available before the end
     *  of file, from the textual input port into a newly allocated string in
     *  left-to-right order and returns the string. If no characters are
     *  available before the end of file, an end-of-file object is returned.
     *
     * ---------------------------------------------------------------------- */
    // TODO string(std::istream &, size_type k);

    operator codeunits() const;

    auto read(std::istream &) const -> characters;

    auto write_string(std::ostream &) const -> std::ostream &;
  };

  auto operator <<(std::ostream &, string const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
