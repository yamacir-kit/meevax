#include <meevax/kernel/character.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/parser.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto read_codeunit(input_port & port) -> bytestring
  {
    bytestring codeunit {};

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

  auto peek_codeunit(input_port & port) -> bytestring
  {
    const auto position { port.tellg() };

    const auto codeunit { read_codeunit(port) };

    port.seekg(position);

    return codeunit;
  }

  auto character::write_char() const -> bytestring const&
  {
    return static_cast<bytestring const&>(*this);
  }

  auto character::write_char(std::ostream & port) const -> decltype(port)
  {
    return port << write_char();
  }

  auto character::write_char(let const& maybe_port) const -> std::ostream&
  {
    return write_char(maybe_port.as<output_port>());
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
