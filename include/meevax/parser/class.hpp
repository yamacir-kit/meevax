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

#ifndef INCLUDED_MEEVAX_PARSER_CLASS_HPP
#define INCLUDED_MEEVAX_PARSER_CLASS_HPP

#include <type_traits>

namespace meevax
{
  auto is_eof = [](auto c) constexpr
  {
    using character = typename std::char_traits<decltype(c)>;

    return character::eq_int_type(character::to_int_type(c), character::eof());
  };

  auto is_upper = [](auto const& c)
  {
    return 'A' <= c[0] and c[0] <= 'Z';
  };

  auto is_lower = [](auto const& c)
  {
    return 'a' <= c[0] and c[0] <= 'z';
  };

  auto is_letter = [](auto const& c)
  {
    return is_upper(c) or is_lower(c);
  };
} // namespace meevax

#endif // INCLUDED_MEEVAX_PARSER_CLASS_HPP
