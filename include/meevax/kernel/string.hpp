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

    /*
       (make-string k)                                                procedure
       (make-string k char)                                           procedure

       The make-string procedure returns a newly allocated string of length k.
       If char is given, then all the characters of the string are initialized
       to char, otherwise the contents of the string are unspecified.
    */
    explicit string(const_reference k, const_reference c)
      : codepoints { k.as<exact_integer>(), c.as<character>() }
    {}

    auto copy(const_reference, const_reference) const -> value_type;

    /*
       (string-length string)                                         procedure

       Returns the number of characters in the given string.
    */
    auto length() const -> value_type;

    auto list(std::size_t, std::size_t) const -> meevax::value_type;

    auto list(std::size_t = 0) const -> meevax::value_type;

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
