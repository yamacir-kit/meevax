#include <iomanip>

#include <meevax/iostream/ignore.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/parser.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  auto string::read(input_port & port) const -> characters
  {
    auto hex_scalar = [](input_port & port)
    {
      if (std::string token; std::getline(port, token, ';') and port.ignore(1))
      {
        if (std::stringstream ss; ss << std::hex << token)
        {
          if (codepoint value = 0; ss >> value)
          {
            return value;
          }
        }
      }

      throw read_error<character>("invalid escape sequence");
    };

    characters cs;

    for (auto c = character(port); not is_eof(c.value); c = character(port))
    {
      switch (c.value)
      {
      case '"':
        return cs;

      case '\\':
        switch (auto const c = character(port); c.value)
        {
        case 'a': cs.emplace_back('\a'); break;
        case 'b': cs.emplace_back('\b'); break;
        case 'f': cs.emplace_back('\f'); break;
        case 'n': cs.emplace_back('\n'); break;
        case 'r': cs.emplace_back('\r'); break;
        case 't': cs.emplace_back('\t'); break;
        case 'v': cs.emplace_back('\v'); break;

        case 'x':
          cs.emplace_back(hex_scalar(port));
          break;

        case '\n':
        case '\r':
          ignore(port, is_intraline_whitespace);
          break;

        default:
          cs.push_back(c);
          break;
        }
        break;

      default:
        cs.push_back(c);
        break;
      }
    }

    throw read_error<string>("unterminated string");
  }

  auto string::write_string(output_port & port) const -> output_port &
  {
    return port << static_cast<codeunits const&>(*this);
  }

  auto operator <<(output_port & port, string const& datum) -> output_port &
  {
    auto print = [&](character const& c) -> decltype(auto)
    {
      if (c.value < 0x80)
      {
        switch (c.value)
        {
        case '\a': return port << red << "\\a";
        case '\b': return port << red << "\\b";
        case '\t': return port << red << "\\t";
        case '\n': return port << red << "\\n";
        case '\r': return port << red << "\\r";
        case '\"': return port << red << "\\\"";
        case '\\': return port << red << "\\\\";
        case '|':  return port << red << "\\|";

        default:
          return port << cyan << static_cast<char>(c.value);
        }
      }
      else
      {
        return port << red << "\\x" << std::hex << std::uppercase << c.value << ";";
      }
    };

    port << cyan << "\"";

    for (auto const& each : datum)
    {
      print(each);
    }

    return port << cyan << "\"" << reset;
  }
} // namespace kernel
} // namespace meevax
