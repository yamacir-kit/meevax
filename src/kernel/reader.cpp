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

#include <meevax/kernel/reader.hpp>

namespace meevax
{
inline namespace kernel
{
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
} // namespace kernel
} // namespace meevax
