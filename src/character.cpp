#include <meevax/kernel/character.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax { inline namespace kernel
{
  auto encode(std::uint_least32_t code) -> std::string
  {
    char sequence[5] = {};

    if (code <= 0x7F)
    {
      sequence[1] = '\0';
      sequence[0] = (code & 0x7F);
    }
    else if (code <= 0x7FF)
    {
      sequence[2] = '\0';
      sequence[1] = 0x80 | (code & 0x3F); code >>= 6;
      sequence[0] = 0xC0 | (code & 0x1F);
    }
    else if (code <= 0xFFFF)
    {
      sequence[3] = '\0';
      sequence[2] = 0x80 | (code & 0x3F); code >>= 6;
      sequence[1] = 0x80 | (code & 0x3F); code >>= 6;
      sequence[0] = 0xE0 | (code & 0x0F);
    }
    else if (code <= 0x10FFFF)
    {
      sequence[4] = '\0';
      sequence[3] = 0x80 | (code & 0x3F); code >>= 6;
      sequence[2] = 0x80 | (code & 0x3F); code >>= 6;
      sequence[1] = 0x80 | (code & 0x3F); code >>= 6;
      sequence[0] = 0xF0 | (code & 0x07);
    }
    else
    {
      sequence[3] = '\0';
      sequence[2] = 0xEF;
      sequence[1] = 0xBF;
      sequence[0] = 0xBD;
    }

    return sequence;
  }

  auto character::display_to(std::ostream& port) const -> decltype(port)
  {
    return port << display() << reset;
  }

  auto character::display_to(let const& maybe_port) const -> std::ostream&
  {
    return display_to(maybe_port.as<output_port>());
  }

  auto operator <<(std::ostream& port, const character& datum) -> decltype(port)
  {
    port << cyan << "#\\";

    switch (std::size(datum))
    {
    case 1:
      switch (datum[0])
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
        return port << datum.display() << reset;
      }

    default:
      return port << datum.display() << reset;
    }
  }
}} // namespace meevax::kernel
