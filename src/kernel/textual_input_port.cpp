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

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/textual_input_port.hpp>

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

  auto textual_input_port::get() -> object
  {
    try
    {
      return make<character>(take_codepoint());
    }
    catch (eof const&)
    {
      return eof_object;
    }
  }

  auto textual_input_port::get(std::size_t size) -> object
  {
    try
    {
      auto s = string();

      for (std::size_t i = 0; i < size; ++i)
      {
        s.codepoints.emplace_back(take_codepoint());
      }

      return make(s);
    }
    catch (eof const&)
    {
      return eof_object;
    }
  }

  auto textual_input_port::get_line() -> object
  {
    if (auto s = std::string(); std::getline(static_cast<std::istream &>(*this), s).eof())
    {
      return eof_object;
    }
    else
    {
      return make<string>(s);
    }
  }

  auto textual_input_port::get_ready() const -> bool
  {
    return static_cast<bool>(static_cast<std::istream const&>(*this));
  }

  auto textual_input_port::peek() -> object
  {
    try
    {
      auto g = static_cast<std::istream &>(*this).tellg();
      let c = make<character>(take_codepoint());
      static_cast<std::istream &>(*this).seekg(g);
      return c;
    }
    catch (eof const&)
    {
      return eof_object;
    }
  }

  auto textual_input_port::read() -> object
  {
    try
    {
      return interaction_environment().as<environment>().read(*this);
    }
    catch (eof const&)
    {
      return eof_object;
    }
  }

  auto textual_input_port::take_codepoint() -> character::int_type
  {
    /*
       00000000 -- 0000007F: 0xxxxxxx
       00000080 -- 000007FF: 110xxxxx 10xxxxxx
       00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
       00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    */

    character::int_type codepoint = 0;

    auto & istream = static_cast<std::istream &>(*this);

    if (auto const c = istream.peek(); character::is_eof(c))
    {
      throw eof();
    }
    else if (0x00 <= c and c <= 0x7F) // 7 bit
    {
      codepoint = istream.get();
    }
    else if (0xC2 <= c and c <= 0xDF) // 11 bit
    {
      codepoint |= istream.get() bitand 0b0001'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111;
    }
    else if (0xE0 <= c and c <= 0xEF) // 16 bit
    {
      codepoint |= istream.get() bitand 0b0000'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111;
    }
    else if (0xF0 <= c and c <= 0xF4) // 21 bit
    {
      codepoint |= istream.get() bitand 0b0000'0111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111;
    }
    else
    {
      throw read_error(make<string>("An end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable"));
    }

    return codepoint;
  }

  auto textual_input_port::take_digits() -> std::string
  {
    auto digits = std::string();

    for (auto & istream = static_cast<std::istream &>(*this);
         std::isdigit(istream.peek());
         digits.push_back(istream.get()));

    return std::empty(digits) ? "0" : digits;
  }

  auto textual_input_port::take_nested_block_comment() -> void
  {
    auto & istream = static_cast<std::istream &>(*this);

    while (not character::is_eof(istream.peek()))
    {
      switch (istream.get())
      {
      case '#':
        switch (istream.peek())
        {
        case '|':
          istream.ignore(1);
          take_nested_block_comment();
          [[fallthrough]];

        default:
          continue;
        }

      case '|':
        switch (istream.peek())
        {
        case '#':
          istream.ignore(1);
          return;

        default:
          continue;
        }

      default:
        continue;
      }
    }

    throw read_error(make<string>("An end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable"));
  }

  auto textual_input_port::take_token() -> std::string
  {
    auto token = std::string();

    for (auto & istream = static_cast<std::istream &>(*this);
         not is_special_character(istream.peek());
         token.push_back(istream.get()));

    return token;
  }
} // namespace kernel
} // namespace meevax
