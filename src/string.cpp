#include <iomanip>

#include <meevax/iostream/ignore.hpp>
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

  auto string::write_string() const -> std::string
  {
    output_string_port port {};

    car(*this).as<character>().write(port);

    for (auto const& each : cdr(*this))
    {
      each.as<character>().write(port);
    }

    return port.str();
  }

  auto string::write_string(output_port& port) const -> output_port &
  {
    return port << write_string();
  }

  auto string::write_string(let const& x) const -> output_port &
  {
    return write_string(x.as<output_port>());
  }

  bool operator==(const string& lhs, const string& rhs)
  {
    return lhs.write_string() == rhs.write_string();
  }

  auto operator <<(output_port& port, const string& datum) -> output_port &
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

    print(car(datum).as<character>());

    for (let const& each : cdr(datum))
    {
      print(each.as<character>());
    }

    return port << cyan << "\"" << reset;
  }
} // namespace kernel
} // namespace meevax
