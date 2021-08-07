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

#include <meevax/kernel/reader.hpp>

namespace meevax
{
inline namespace kernel
{
  auto read_token(std::istream & is) -> std::string
  {
    std::string token;

    for (auto c = is.peek(); not is_end_of_token(c); c = is.peek())
    {
      token.push_back(is.get());
    }

    return token;
  }

  /* ---- R7RS 7.1.1. Lexical structure ----------------------------------------
   *
   *  <character> = #\ <any character>
   *              | #\ <character name>
   *              | #\x <hex scalar value>
   *              | #\U+ <hex scalar value> TODO
   *
   *  <character name> = alarm
   *                   | backspace
   *                   | delete
   *                   | escape
   *                   | newline
   *                   | null
   *                   | return
   *                   | space
   *                   | tab
   *
   * ------------------------------------------------------------------------ */
  auto read_char(std::istream & is) -> pair::value_type
  {
    auto any_character = [&](auto const& token, auto)
    {
      switch (token.size())
      {
      case 0:
        return make<character>(is.get());

      case 1:
        return make<character>(token.front());

      default:
        throw tagged_read_error<character>(
          make<string>(
            "If <character> in #\\<character> is alphabetic, then any "
            "character immediately following <character> cannot be one that "
            "can appear in an identifier"),
          unit);
      }
    };

    auto character_name = [](auto const& token, auto)
    {
      std::unordered_map<std::string, char> static const names
      {
        { "alarm"    , 0x07 },
        { "backspace", 0x08 },
        { "delete"   , 0x7F },
        { "escape"   , 0x1B },
        { "newline"  , 0x0A },
        { "null"     , 0x00 },
        { "return"   , 0x0D },
        { "space"    , 0x20 },
        { "tab"      , 0x09 },
      };

      return make<character>(names.at(token));
    };

    auto hex_scalar_value = [](auto const& token, auto = 16)
    {
      if (token.front() == 'x' and 1 < token.size())
      {
        std::stringstream ss;
        ss << std::hex << token.substr(1);

        character::value_type value = 0;
        ss >> value;

        return make<character>(value);
      }
      else
      {
        throw tagged_read_error<character>(
          make<string>("invalid character literal: "),
          make<string>("\\#" + token));
      }
    };

    auto to_character = hex_scalar_value | character_name | any_character;

    return to_character(read_token(is), 16);
  }
} // namespace kernel
} // namespace meevax
