#include <meevax/kernel/character.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/parser.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax
{
inline namespace kernel
{
  auto codepoint_to_codeunit(std::uint_least32_t codepoint) -> std::string
  {
    char sequence[5] = {};

    if (codepoint <= 0x7F)
    {
      sequence[1] = '\0';
      sequence[0] = (codepoint & 0x7F);
    }
    else if (codepoint <= 0x7FF)
    {
      sequence[2] = '\0';
      sequence[1] = 0x80 | (codepoint & 0x3F); codepoint >>= 6;
      sequence[0] = 0xC0 | (codepoint & 0x1F);
    }
    else if (codepoint <= 0xFFFF)
    {
      sequence[3] = '\0';
      sequence[2] = 0x80 | (codepoint & 0x3F); codepoint >>= 6;
      sequence[1] = 0x80 | (codepoint & 0x3F); codepoint >>= 6;
      sequence[0] = 0xE0 | (codepoint & 0x0F);
    }
    else if (codepoint <= 0x10FFFF)
    {
      sequence[4] = '\0';
      sequence[3] = 0x80 | (codepoint & 0x3F); codepoint >>= 6;
      sequence[2] = 0x80 | (codepoint & 0x3F); codepoint >>= 6;
      sequence[1] = 0x80 | (codepoint & 0x3F); codepoint >>= 6;
      sequence[0] = 0xF0 | (codepoint & 0x07);
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

  auto codeunit_to_codepoint(std::string const& code) -> std::uint_least32_t
  {
    std::uint_least32_t codepoint {};

    /* -------------------------------------------------------------------------
     *
     *  00000000 -- 0000007F: 0xxxxxxx
     *  00000080 -- 000007FF: 110xxxxx 10xxxxxx
     *  00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
     *  00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
     *
     * ---------------------------------------------------------------------- */

    switch (std::size(code))
    {
    case 1:
      codepoint |= code[0] & 0b0111'1111;
      break;

    case 2:
      codepoint |= code[0] & 0b0001'1111; codepoint <<= 6;
      codepoint |= code[1] & 0b0011'1111;
      break;

    case 3:
      codepoint |= code[0] & 0b0000'1111; codepoint <<= 6;
      codepoint |= code[1] & 0b0011'1111; codepoint <<= 6;
      codepoint |= code[2] & 0b0011'1111;
      break;

    case 4:
      codepoint |= code[0] & 0b0000'0111; codepoint <<= 6;
      codepoint |= code[1] & 0b0011'1111; codepoint <<= 6;
      codepoint |= code[2] & 0b0011'1111; codepoint <<= 6;
      codepoint |= code[3] & 0b0011'1111;
      break;

    default:
      throw error("Malformed character.");
    }

    return codepoint;
  }

  auto read_codeunit(input_port & port) -> std::string
  {
    std::string codeunit {};

    if (const auto c { port.peek() }; is_end_of_file(c))
    {
      throw read_error<eof>("exhausted input-port");
    }
    else if (0b1111'0000 < c)
    {
      codeunit.push_back(port.narrow(port.get(), 'A'));
      codeunit.push_back(port.narrow(port.get(), 'B'));
      codeunit.push_back(port.narrow(port.get(), 'C'));
      codeunit.push_back(port.narrow(port.get(), 'D'));
    }
    else if (0b1110'0000 < c)
    {
      codeunit.push_back(port.narrow(port.get(), 'A'));
      codeunit.push_back(port.narrow(port.get(), 'B'));
      codeunit.push_back(port.narrow(port.get(), 'C'));
    }
    else if (0b1100'0000 < c)
    {
      codeunit.push_back(port.narrow(port.get(), 'A'));
      codeunit.push_back(port.narrow(port.get(), 'B'));
    }
    else
    {
      codeunit.push_back(port.narrow(port.get(), 'A'));
    }

    return codeunit;
  }

  auto peek_codeunit(input_port & port) -> std::string
  {
    const auto position { port.tellg() };

    const auto codeunit { read_codeunit(port) };

    port.seekg(position);

    return codeunit;
  }

  auto character::write_char() const -> std::string const&
  {
    return static_cast<std::string const&>(*this);
  }

  auto character::write_char(std::ostream & port) const -> decltype(port)
  {
    return port << write_char();
  }

  auto character::write_char(let const& maybe_port) const -> std::ostream&
  {
    return write_char(maybe_port.as<output_port>());
  }

  auto operator <<(std::ostream& port, character const& datum) -> decltype(port)
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
        return datum.write_char(port) << reset;
      }

    default:
      return datum.write_char(port) << reset;
    }
  }
} // namespace kernel
} // namespace meevax
