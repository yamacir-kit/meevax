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
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/parser.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  static_assert(std::alignment_of<character>::value == std::alignment_of<codepoint>::value);
  static_assert(std::is_trivial<character>::value);
  static_assert(std::is_standard_layout<character>::value);
  static_assert(std::is_pod<character>::value);

  auto character::read_codeunit(input_port & port) const -> codeunit
  {
    codeunit cu {};

    if (auto const c = port.peek(); is_eof(c))
    {
      throw tagged_read_error<eof>(
        make<string>("no more characters are available"), unit);
    }
    else if (0b1111'0000 < c)
    {
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
    }
    else if (0b1110'0000 < c)
    {
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
    }
    else if (0b1100'0000 < c)
    {
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
    }
    else
    {
      cu.push_back(port.narrow(port.get(), '\0'));
    }

    return cu;
  }

  auto character::read(input_port & port) const -> codepoint
  {
    /* -------------------------------------------------------------------------
     *
     *  00000000 -- 0000007F: 0xxxxxxx
     *  00000080 -- 000007FF: 110xxxxx 10xxxxxx
     *  00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
     *  00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
     *
     * ---------------------------------------------------------------------- */

    codepoint point = 0;

    if (auto const c = port.peek(); is_eof(c))
    {
      throw tagged_read_error<eof>(
        make<string>("no more characters are available"), unit);
    }
    else if (0b1111'0000 < c)
    {
      point |= port.get() & 0b0000'0111; point <<= 6;
      point |= port.get() & 0b0011'1111; point <<= 6;
      point |= port.get() & 0b0011'1111; point <<= 6;
      point |= port.get() & 0b0011'1111;
    }
    else if (0b1110'0000 < c)
    {
      point |= port.get() & 0b0000'1111; point <<= 6;
      point |= port.get() & 0b0011'1111; point <<= 6;
      point |= port.get() & 0b0011'1111;
    }
    else if (0b1100'0000 < c)
    {
      point |= port.get() & 0b0001'1111; point <<= 6;
      point |= port.get() & 0b0011'1111;
    }
    else // is ascii
    {
      point |= port.get() & 0b0111'1111;
    }

    return point;
  }

  auto character::write(output_port & port) const -> output_port &
  {
    return port << static_cast<codeunit const&>(*this);
  }

  auto operator <<(output_port & port, character const& datum) -> output_port &
  {
    port << cyan << "#\\";

    switch (datum.value)
    {
    case 0x00: return port << "null"      << reset;
    case 0x07: return port << "alarm"     << reset;
    case 0x08: return port << "backspace" << reset;
    case 0x09: return port << "tab"       << reset;
    case 0x0A: return port << "newline"   << reset;
    case 0x0D: return port << "return"    << reset;
    case 0x1B: return port << "escape"    << reset;
    case 0x20: return port << "space"     << reset;
    case 0x7F: return port << "delete"    << reset;

    default:
      return datum.write(port) << reset;
    }
  }
} // namespace kernel
} // namespace meevax
