#include <iomanip>

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  bool operator==(const string& lhs, const string& rhs)
  {
    return static_cast<std::string>(lhs) == static_cast<std::string>(rhs);
  }

  output_port& operator <<(output_port& port, const string& datum)
  {
    auto write_char = [&](const character& code) -> decltype(auto)
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

    write_char(car(datum).as<character>());

    for (let const& each : cdr(datum))
    {
      write_char(each.as<character>());
    }

    return port << cyan << "\"" << reset;
  }
} // namespace kernel
} // namespace meevax
