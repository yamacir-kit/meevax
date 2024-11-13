/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  character::operator std::string() const
  {
    auto chars = std::array<char, 5>();

    if (codepoint <= 0x7F)
    {
      chars[0] = (codepoint & 0x7F);
    }
    else if (codepoint <= 0x7FF)
    {
      chars[1] = 0x80 | (codepoint >>  0 & 0x3F);
      chars[0] = 0xC0 | (codepoint >>  6 & 0x1F);
    }
    else if (codepoint <= 0xFFFF)
    {
      chars[2] = 0x80 | (codepoint >>  0 & 0x3F);
      chars[1] = 0x80 | (codepoint >>  6 & 0x3F);
      chars[0] = 0xE0 | (codepoint >> 12 & 0x0F);
    }
    else if (codepoint <= 0x10FFFF)
    {
      chars[3] = 0x80 | (codepoint >>  0 & 0x3F);
      chars[2] = 0x80 | (codepoint >>  6 & 0x3F);
      chars[1] = 0x80 | (codepoint >> 12 & 0x3F);
      chars[0] = 0xF0 | (codepoint >> 18 & 0x07);
    }
    else
    {
      chars[2] = std::char_traits<char_type>::to_char_type(0xEF);
      chars[1] = std::char_traits<char_type>::to_char_type(0xBF);
      chars[0] = std::char_traits<char_type>::to_char_type(0xBD);
    }

    return chars.data();
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
      return os << cyan(static_cast<std::string>(datum));
    }
  }
} // namespace kernel
} // namespace meevax
