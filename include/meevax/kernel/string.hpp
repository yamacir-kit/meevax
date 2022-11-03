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
       (make-string k)                                                procedure
       (make-string k char)                                           procedure

       The make-string procedure returns a newly allocated string of length k.
       If char is given, then all the characters of the string are initialized
       to char, otherwise the contents of the string are unspecified.
    */
    explicit string(const_reference, const_reference);

    /*
       (string-length string)                                         procedure

       Returns the number of characters in the given string.
    */
    auto length() const -> value_type;

    operator std::string() const; // write-string (for display)
  };

  auto operator ==(string const&, string const&) -> bool;

  auto operator <<(std::ostream &, string const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STRING_HPP
