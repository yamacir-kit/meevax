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

#include <array>
#include <stdexcept>

#include <meevax/string/unicode.hpp>

namespace meevax
{
  auto codepoint_to_codeunit(codepoint cp) -> codeunit
  {
    std::array<codeunit::value_type, 5> cu = {};

    if (cp <= 0x7F)
    {
      cu[1] = '\0';
      cu[0] = (cp & 0x7F);
    }
    else if (cp <= 0x7FF)
    {
      cu[2] = '\0';
      cu[1] = 0x80 | (cp & 0x3F); cp >>= 6;
      cu[0] = 0xC0 | (cp & 0x1F);
    }
    else if (cp <= 0xFFFF)
    {
      cu[3] = '\0';
      cu[2] = 0x80 | (cp & 0x3F); cp >>= 6;
      cu[1] = 0x80 | (cp & 0x3F); cp >>= 6;
      cu[0] = 0xE0 | (cp & 0x0F);
    }
    else if (cp <= 0x10FFFF)
    {
      cu[4] = '\0';
      cu[3] = 0x80 | (cp & 0x3F); cp >>= 6;
      cu[2] = 0x80 | (cp & 0x3F); cp >>= 6;
      cu[1] = 0x80 | (cp & 0x3F); cp >>= 6;
      cu[0] = 0xF0 | (cp & 0x07);
    }
    else
    {
      cu[3] = '\0';
      cu[2] = static_cast<codeunit::value_type>(0xEF);
      cu[1] = static_cast<codeunit::value_type>(0xBF);
      cu[0] = static_cast<codeunit::value_type>(0xBD);
    }

    return cu.data();
  }

  auto codeunit_to_codepoint(codeunit const& cu) -> codepoint
  {
    codepoint cp {};

    /* -------------------------------------------------------------------------
     *
     *  00000000 -- 0000007F: 0xxxxxxx
     *  00000080 -- 000007FF: 110xxxxx 10xxxxxx
     *  00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
     *  00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
     *
     * ---------------------------------------------------------------------- */

    switch (std::size(cu))
    {
    case 1:
      cp |= cu[0] & 0b0111'1111;
      break;

    case 2:
      cp |= cu[0] & 0b0001'1111; cp <<= 6;
      cp |= cu[1] & 0b0011'1111;
      break;

    case 3:
      cp |= cu[0] & 0b0000'1111; cp <<= 6;
      cp |= cu[1] & 0b0011'1111; cp <<= 6;
      cp |= cu[2] & 0b0011'1111;
      break;

    case 4:
      cp |= cu[0] & 0b0000'0111; cp <<= 6;
      cp |= cu[1] & 0b0011'1111; cp <<= 6;
      cp |= cu[2] & 0b0011'1111; cp <<= 6;
      cp |= cu[3] & 0b0011'1111;
      break;

    default:
      throw std::runtime_error("Malformed character.");
    }

    return cp;
  }
} // namespace meevax
