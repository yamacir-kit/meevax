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
  struct string
  {
    std::vector<character> codepoints;

    explicit string() = default;

    explicit string(std::string const&);

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
       (vector->string vector)                                        procedure
       (vector->string vector start)                                  procedure
       (vector->string vector start end)                              procedure

       It is an error if any element of vector between start and end is not a
       character.

       The vector->string procedure returns a newly allocated string of the
       objects contained in the elements of vector between start and end. The
       string->vector procedure returns a newly created vector initialized to
       the elements of the string string between start and end.

       In both procedures, order is preserved.
    */
    explicit string(const_reference, const_reference, const_reference);

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

    operator std::string() const; // write-string (for display)
  };

  auto operator ==(string const&, string const&) -> bool;

  auto operator <<(std::ostream &, string const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
