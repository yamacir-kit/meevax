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

#include <meevax/iostream/ignore.hpp>
#include <meevax/kernel/reader.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Char>
  auto is_special_character(Char c)
  {
    auto one_of = [c](auto... xs) constexpr
    {
      return (std::char_traits<char>::eq(c, xs) or ...);
    };

    return one_of(std::char_traits<char>::eof(),
                  '\t', // 0x09
                  '\n', // 0x0A
                  '\v', // 0x0B
                  '\f', // 0x0C
                  '\r', // 0x0D
                  ' ',  // 0x20
                  '"',  // 0x22
                  '#',  // 0x23
                  '\'', // 0x27
                  '(',  // 0x28
                  ')',  // 0x29
                  ',',  // 0x2C
                  ';',  // 0x3B
                  '[',  // 0x5B
                  ']',  // 0x5D
                  '`',  // 0x60
                  '{',  // 0x7B
                  '|',  // 0x7C
                  '}'); // 0x7D
  }

  auto read_character(std::istream & is) -> value_type
  {
    std::unordered_map<external_representation, character::int_type> static const character_names {
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

    switch (auto token = read_token(is); token.length())
    {
    case 0:
      assert(is_special_character(is.peek()));
      return make<character>(is.get());

    case 1:
      assert(std::isprint(token.front()));
      return make<character>(token.front());

    default:
      if (auto iter = character_names.find(token); iter != std::end(character_names))
      {
        return make<character>(iter->second);
      }
      else if (token[0] == 'x' and 1 < token.length())
      {
        return make<character>(lexical_cast<character::int_type>(std::hex, token.substr(1)));
      }
      else
      {
        for (auto iter = std::rbegin(token); iter != std::rend(token); ++iter)
        {
          is.putback(*iter);
        }

        throw read_error(make<string>("not a character"), make<string>("\\#" + token));
      }
    }
  }

  auto read_codepoint(std::istream & is) -> character::int_type /* -------------
  *
  *  00000000 -- 0000007F: 0xxxxxxx
  *  00000080 -- 000007FF: 110xxxxx 10xxxxxx
  *  00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
  *  00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
  *
  * ------------------------------------------------------------------------- */
  {
    character::int_type codepoint = 0;

    if (auto const c = is.peek(); std::char_traits<char>::eq(std::char_traits<char>::eof(), c))
    {
      throw eof();
    }
    else if (0x00 <= c and c <= 0x7F) // 7 bit
    {
      codepoint = is.get();
    }
    else if (0xC2 <= c and c <= 0xDF) // 11 bit
    {
      codepoint |= is.get() bitand 0b0001'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111;
    }
    else if (0xE0 <= c and c <= 0xEF) // 16 bit
    {
      codepoint |= is.get() bitand 0b0000'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111;
    }
    else if (0xF0 <= c and c <= 0xF4) // 21 bit
    {
      codepoint |= is.get() bitand 0b0000'0111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111;
    }
    else
    {
      throw read_error(make<string>("invalid stream"), unit);
    }

    return codepoint;
  }

  auto read_comment(std::istream & is) -> std::istream &
  {
    while (not std::char_traits<char>::eq(std::char_traits<char>::eof(), is.peek())) switch (is.get())
    {
    case '#':
      switch (is.peek())
      {
      case '|':
        is.ignore(1);
        read_comment(is);
        [[fallthrough]];

      default:
        continue;
      }

    case '|':
      switch (is.peek())
      {
      case '#':
        is.ignore(1);
        return is;

      default:
        continue;
      }

    default:
      continue;
    }

    throw read_error(make<string>("unterminated multi-line comment"), unit);
  }

  auto read_string(std::istream & is) -> value_type
  {
    let const s = make<string>();

    auto&& codepoints = s.as<string>().codepoints;

    for (auto codepoint = read_codepoint(is); not std::char_traits<char>::eq(std::char_traits<char>::eof(), codepoint); codepoint = read_codepoint(is))
    {
      switch (codepoint)
      {
      case '"':
        return s;

      case '\\':
        switch (auto const codepoint = read_codepoint(is); codepoint)
        {
        case 'a': codepoints.emplace_back('\a'); break;
        case 'b': codepoints.emplace_back('\b'); break;
        case 'f': codepoints.emplace_back('\f'); break;
        case 'n': codepoints.emplace_back('\n'); break;
        case 'r': codepoints.emplace_back('\r'); break;
        case 't': codepoints.emplace_back('\t'); break;
        case 'v': codepoints.emplace_back('\v'); break;

        case 'x':
          if (auto token = external_representation(); std::getline(is, token, ';') and is.ignore(1))
          {
            if (std::stringstream ss; ss << std::hex << token)
            {
              if (character::int_type value = 0; ss >> value)
              {
                codepoints.emplace_back(value);
                break;
              }
            }
          }
          throw read_error(make<string>("invalid escape sequence"));

        case '\n':
        case '\r':
          ignore(is, [](auto c) { return std::isspace(c); });
          break;

        default:
          codepoints.emplace_back(codepoint);
          break;
        }
        break;

      default:
        codepoints.emplace_back(codepoint);
        break;
      }
    }

    throw read_error(make<string>("unterminated string"), unit);
  }

  auto read_token(std::istream & is) -> std::string
  {
    auto token = std::string();

    while (not is_special_character(is.peek()))
    {
      token.push_back(is.get());
    }

    return token;
  }
} // namespace kernel
} // namespace meevax
