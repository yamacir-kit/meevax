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

    /*
       (list->string list)                                            procedure

       It is an error if any element of list is not a character.

       The string->list procedure returns a newly allocated list of the
       characters of string between start and end. list->string returns a newly
       allocated string formed from the elements in the list list. In both
       procedures, order is preserved. string->list and list->string are
       inverses so far as equal? is concerned.
    */
    explicit string(const_reference);

    /*
       (make-string k)                                                procedure
       (make-string k char)                                           procedure

       The make-string procedure returns a newly allocated string of length k.
       If char is given, then all the characters of the string are initialized
       to char, otherwise the contents of the string are unspecified.
    */
    explicit string(const_reference, const_reference);

    /*
       (string-append string ...)                                     procedure

       Returns a newly allocated string whose characters are the concatenation
       of the characters in the given strings.
    */
    static auto append(const_reference) -> value_type;

    /*
       (string-copy string)                                           procedure
       (string-copy string start)                                     procedure
       (string-copy string start end)                                 procedure

       Returns a newly allocated copy of the part of the given string between
       start and end.
    */
    auto copy(const_reference, const_reference) const -> value_type;

    /*
       (string-copy! to at from)                                      procedure
       (string-copy! to at from start)                                procedure
       (string-copy! to at from start end)                            procedure

       It is an error if at is less than zero or greater than the length of to.
       It is also an error if (- (string-length to) at) is less than (- end
       start).

       Copies the characters of string from between start and end to string to,
       starting at at. The order in which characters are copied is unspecified,
       except that if the source and destination overlap, copying takes place
       as if the source is first copied into a temporary string and then into
       the destination. This can be achieved without allocating storage by
       making sure to copy in the correct direction in such circumstances.
    */
    auto copy(const_reference, const_reference, const_reference, const_reference) -> void;

    /*
       (string-length string)                                         procedure

       Returns the number of characters in the given string.
    */
    auto length() const -> value_type;

    /*
       (string->list string)                                          procedure
       (string->list string start)                                    procedure
       (string->list string start end)                                procedure

       (list->string list)                                            procedure

       It is an error if any element of list is not a character.

       The string->list procedure returns a newly allocated list of the
       characters of string between start and end. list->string returns a newly
       allocated string formed from the elements in the list list. In both
       procedures, order is preserved. string->list and list->string are
       inverses so far as equal? is concerned.
    */
    auto make_list(const_reference, const_reference) const -> value_type;

    /*
       (string-ref string k)                                          procedure

       It is an error if k is not a valid index of string.

       The string-ref procedure returns character k of string using zero-origin
       indexing. There is no requirement for this procedure to execute in
       constant time.
    */
    auto ref(const_reference) const -> value_type;

    /*
       (string-set! string k char)                                    procedure

       It is an error if k is not a valid index of string.

       The string-set! procedure stores char in element k of string. There is
       no requirement for this procedure to execute in constant time.
    */
    auto set(const_reference, const_reference) -> void;

    operator external_representation() const; // write-string (for display)
  };

  auto operator ==(string const&, string const&) -> bool;

  auto operator <<(std::ostream &, string const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
