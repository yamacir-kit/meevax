#include <iomanip>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  auto string::write_string() const -> std::string
  {
    output_string_port port {};

    car(*this).as<character>().write_char(port);

    for (auto const& each : cdr(*this))
    {
      each.as<character>().write_char(port);
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
    auto print = [&](const character& code) -> decltype(auto)
    {
      switch (std::size(code))
      {
      case 1:
        switch (code[0])
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
          return port << cyan << code.write_char();
        }

      default:
        return port << red << "\\x" << std::hex << std::uppercase << code.codepoint() << ";";
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
