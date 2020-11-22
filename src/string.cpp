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
    port << cyan << "\"" << car(datum).as<character>().display();

    for (let const& each : cdr(datum))
    {
      if (not each.is<null>()) // guard for malformed string
      {
        switch (const auto code { each.as<character>().display() }; code[0])
        {
        case '\n': port << red << "\\n"; break;
        case '\t': port << red << "\\t"; break;

        default:
          port << cyan << code;
          break;
        }
      }
    }

    return port << cyan << "\"" << reset;
  }
} // namespace kernel
} // namespace meevax
