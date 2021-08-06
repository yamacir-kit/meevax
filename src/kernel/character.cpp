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

#include <meevax/kernel/character.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/miscellaneous.hpp> // for eof
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/parser.hpp>
#include <meevax/posix/vt10x.hpp> // for cyan

namespace meevax
{
inline namespace kernel
{
  character::character(std::char_traits<char>::int_type const codepoint)
    : codepoint { codepoint }
  {}

  character::character(std::istream & is)
    : codepoint {}
  {
    /*
       00000000 -- 0000007F: 0xxxxxxx
       00000080 -- 000007FF: 110xxxxx 10xxxxxx
       00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
       00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    */

    if (auto const c = is.peek(); is_eof(c))
    {
      throw tagged_read_error<eof>(make<string>("no more characters are available"), unit);
    }
    else if (0b1111'0000 < c)
    {
      codepoint |= is.get() & 0b0000'0111; codepoint <<= 6;
      codepoint |= is.get() & 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() & 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() & 0b0011'1111;
    }
    else if (0b1110'0000 < c)
    {
      codepoint |= is.get() & 0b0001'1111; codepoint <<= 6;
      codepoint |= is.get() & 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() & 0b0011'1111;
    }
    else if (0b1100'0000 < c)
    {
      codepoint |= is.get() & 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() & 0b0011'1111;
    }
    else // ascii
    {
      codepoint |= is.get() & 0b0111'1111;
    }
  }

  character::operator std::char_traits<char>::int_type() const
  {
    return codepoint;
  }

  character::operator codeunit() const
  {
    return codepoint_to_codeunit(codepoint);
  }

  auto character::write(std::ostream & os) const -> std::ostream &
  {
    return os << static_cast<codeunit const&>(*this);
  }

  auto operator <<(std::ostream & os, character const& datum) -> std::ostream &
  {
    os << cyan << "#\\";

    switch (datum.codepoint)
    {
    case 0x00: return os << "null"      << reset;
    case 0x07: return os << "alarm"     << reset;
    case 0x08: return os << "backspace" << reset;
    case 0x09: return os << "tab"       << reset;
    case 0x0A: return os << "newline"   << reset;
    case 0x0D: return os << "return"    << reset;
    case 0x1B: return os << "escape"    << reset;
    case 0x20: return os << "space"     << reset;
    case 0x7F: return os << "delete"    << reset;

    default:
      return datum.write(os) << reset;
    }
  }

  static_assert(std::is_pod<character>::value);

  static_assert(std::is_standard_layout<character>::value);

  static_assert(std::is_trivial<character>::value);
} // namespace kernel
} // namespace meevax
