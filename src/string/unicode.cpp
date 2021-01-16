#include <array>

#include <meevax/string/unicode.hpp>

namespace meevax
{
inline namespace string
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
} // namespace string
} // namespace meevax
