/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#include <regex>

#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  constexpr auto is_special_character(character::int_type c)
  {
    auto one_of = [c](auto... xs) constexpr
    {
      return (character::eq(c, xs) or ...);
    };

    return character::is_eof(c) or one_of('\t', // 0x09
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

  auto get_codepoint(std::istream & is) -> character::int_type /* --------------
  *
  *  00000000 -- 0000007F: 0xxxxxxx
  *  00000080 -- 000007FF: 110xxxxx 10xxxxxx
  *  00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
  *  00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
  *
  * ------------------------------------------------------------------------- */
  {
    character::int_type codepoint = 0;

    if (auto const c = is.peek(); character::is_eof(c))
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
      throw read_error(make<string>("An end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable"));
    }

    return codepoint;
  }

  auto get_token(std::istream & is) -> std::string
  {
    auto token = std::string();

    while (not is_special_character(is.peek()))
    {
      token.push_back(is.get());
    }

    return token;
  }

  auto ignore_nested_block_comment(std::istream & is) -> std::istream &
  {
    while (not character::is_eof(is.peek())) switch (is.get())
    {
    case '#':
      switch (is.peek())
      {
      case '|':
        is.ignore(1);
        ignore_nested_block_comment(is);
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

    throw read_error(make<string>("An end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable"));
  }

  template <>
  auto read<character>(std::istream & is) -> object
  {
    std::unordered_map<std::string, character::int_type> static const character_names {
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

    switch (auto token = get_token(is); token.length())
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

  template <>
  auto read<string>(std::istream & is) -> object
  {
    auto s = string();

    auto const quotation_mark = is.get();

    for (auto codepoint = get_codepoint(is); not character::is_eof(codepoint); codepoint = get_codepoint(is))
    {
      if (codepoint == quotation_mark)
      {
        return make(s);
      }
      else switch (codepoint)
      {
      case '\\':
        switch (auto const codepoint = get_codepoint(is); codepoint)
        {
        case 'a': s.codepoints.emplace_back('\a'); break;
        case 'b': s.codepoints.emplace_back('\b'); break;
        case 'f': s.codepoints.emplace_back('\f'); break;
        case 'n': s.codepoints.emplace_back('\n'); break;
        case 'r': s.codepoints.emplace_back('\r'); break;
        case 't': s.codepoints.emplace_back('\t'); break;
        case 'v': s.codepoints.emplace_back('\v'); break;
        case 'x':
          if (auto token = std::string(); std::getline(is, token, ';'))
          {
            s.codepoints.emplace_back(lexical_cast<character::int_type>(std::hex, token));
          }
          break;

        case '\n':
        case '\r':
          while (std::isspace(is.peek()))
          {
            is.ignore(1);
          }
          break;

        default:
          s.codepoints.emplace_back(codepoint);
          break;
        }
        break;

      default:
        s.codepoints.emplace_back(codepoint);
        break;
      }
    }

    throw read_error(make<string>("An end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable"));
  }

  auto circulate(object const& xs, object const& x, std::string const& n) -> void
  {
    if (xs.is<pair>())
    {
      circulate(car(xs), x, n);

      if (cdr(xs).is<datum_label>() and cdr(xs).as<datum_label>().n == n)
      {
        cdr(xs) = x;
      }
      else
      {
        circulate(cdr(xs), x, n);
      }
    }
  }

  auto circulate(object const& xs, std::string const& n) -> void
  {
    return circulate(xs, xs, n);
  }

  auto make_integer(std::string const& token, int radix) -> object
  {
    return make<exact_integer>(token, radix);
  }

  auto make_rational(std::string const& token, int radix) -> object
  {
    try
    {
      return make_integer(token, radix);
    }
    catch (...)
    {
      return make(ratio(token, radix));
    }
  }

  auto make_real(std::string const& token, int radix) -> object
  {
    try
    {
      return make_rational(token, radix);
    }
    catch (...)
    {
      std::unordered_map<std::string, double> static const constants
      {
        // R7RS 7.1.1. Lexical structure
        { "+inf.0", +std::numeric_limits<double>::infinity()  },
        { "-inf.0", -std::numeric_limits<double>::infinity()  },
        { "+nan.0", +std::numeric_limits<double>::quiet_NaN() },
        { "-nan.0", -std::numeric_limits<double>::quiet_NaN() },

        // SRFI-144
        { "fl-e",         M_E        },
        { "fl-log2-e",    M_LOG2E    },
        { "fl-log10-e",   M_LOG10E   },
        { "fl-log-2",     M_LN2      },
        { "fl-1/log-2",   M_LN2      },
        { "fl-log-10",    M_LN10     },
        { "fl-1/log-10",  M_LN10     },
        { "fl-pi",        M_PI       },
        { "fl-1/pi",      M_1_PI     },
        { "fl-pi/2",      M_PI_2     },
        { "fl-pi/4",      M_PI_4     },
        { "fl-2/pi",      M_2_PI     },
        { "fl-2/sqrt-pi", M_2_SQRTPI },
        { "fl-sqrt-2",    M_SQRT2    },
        { "fl-1/sqrt-2",  M_SQRT1_2  },
      };

      std::regex static const pattern { R"(([+-]?(?:\d+\.?|\d*\.\d+))([DEFLSdefls][+-]?\d+)?)" };

      if (auto iter = constants.find(token); iter != std::end(constants))
      {
        return make(iter->second);
      }
      else if (std::regex_match(token, pattern))
      {
        return make(lexical_cast<double>(token));
      }
      else
      {
        throw std::invalid_argument("not a real number");
      }
    }
  }

  auto make_complex(std::string const& token, int radix) -> object
  {
    try
    {
      return make_real(token, radix);
    }
    catch (...)
    {
      return make(complex(token, radix));
    }
  }

  auto make_number(std::string const& token, int radix) -> object
  {
    try
    {
      return make_complex(token, radix);
    }
    catch (...)
    {
      throw std::invalid_argument("not a number");
    }
  }
} // namespace kernel
} // namespace meevax
