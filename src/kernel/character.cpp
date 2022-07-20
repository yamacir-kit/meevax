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

#include <meevax/kernel/character.hpp>
#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/error.hpp>

namespace meevax
{
inline namespace kernel
{
  character::operator external_representation() const
  {
    std::array<char, 5> bytes {};

    if (auto value = codepoint; value <= 0x7F)
    {
      bytes[0] = (value bitand 0x7F);
    }
    else if (value <= 0x7FF)
    {
      bytes[1] = 0x80 | (value bitand 0x3F); value >>= 6;
      bytes[0] = 0xC0 | (value bitand 0x1F);
    }
    else if (value <= 0xFFFF)
    {
      bytes[2] = 0x80 | (value bitand 0x3F); value >>= 6;
      bytes[1] = 0x80 | (value bitand 0x3F); value >>= 6;
      bytes[0] = 0xE0 | (value bitand 0x0F);
    }
    else if (value <= 0x10FFFF)
    {
      bytes[3] = 0x80 | (value bitand 0x3F); value >>= 6;
      bytes[2] = 0x80 | (value bitand 0x3F); value >>= 6;
      bytes[1] = 0x80 | (value bitand 0x3F); value >>= 6;
      bytes[0] = 0xF0 | (value bitand 0x07);
    }
    else
    {
      bytes[2] = static_cast<char>(0xEF);
      bytes[1] = static_cast<char>(0xBF);
      bytes[0] = static_cast<char>(0xBD);
    }

    return bytes.data();
  }

  auto operator <<(std::ostream & os, character const& datum) -> std::ostream &
  {
    os << cyan("#\\");

    switch (datum.codepoint)
    {
    case 0x00: return os << cyan("null"     );
    case 0x07: return os << cyan("alarm"    );
    case 0x08: return os << cyan("backspace");
    case 0x09: return os << cyan("tab"      );
    case 0x0A: return os << cyan("newline"  );
    case 0x0D: return os << cyan("return"   );
    case 0x1B: return os << cyan("escape"   );
    case 0x20: return os << cyan("space"    );
    case 0x7F: return os << cyan("delete"   );

    default:
      return os << cyan(static_cast<external_representation>(datum));
    }
  }

  static_assert(std::is_pod<character>::value);

  static_assert(std::is_standard_layout<character>::value);

  static_assert(std::is_trivial<character>::value);

  static_assert(4 <= sizeof(character::int_type));
} // namespace kernel
} // namespace meevax
