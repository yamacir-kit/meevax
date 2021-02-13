#include <meevax/kernel/character.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/parser.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto character::read_codeunit(input_port & port) const -> codeunit
  {
    codeunit cu {};

    if (auto const c = port.peek(); is_end_of_file(c))
    {
      throw read_error<eof>("exhausted input-port");
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

  auto character::write_char(output_port & port) const -> output_port &
  {
    return port << static_cast<codeunit const&>(*this);
  }

  auto operator <<(std::ostream & port, character const& datum) -> std::ostream &
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
