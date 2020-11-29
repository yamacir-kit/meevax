#include <meevax/kernel/reader.hpp>

namespace meevax
{
inline namespace kernel
{
  auto read_token(input_port & port) -> std::string
  {
    std::string result {};

    for (auto c { port.peek() }; not is_end_of_token(c); c = port.peek())
    {
      result.push_back(port.get());
    }

    return result;
  }

  let read_char(input_port & port)
  {
    port.ignore(1);

    auto name { read_token(port) };

    if (name.empty())
    {
      name.push_back(port.get());
    }

    static const std::unordered_map<std::string, char> names
    {
      { "alarm"    , 0x07 },
      { "backspace", 0x08 },
      { "delete"   , 0x7F },
      { "escape"   , 0x1B },
      { "newline"  , 0x0A },
      { "null"     , 0x00 },
      { "return"   , 0x0D },
      { "space"    , 0x20 },
      { "tab"      , 0x09 },
    };

    if (const auto iter { names.find(name) }; iter != std::end(names))
    {
      return make<character>(cdr(*iter));
    }
    else
    {
      return make<character>(name);
    }
  }

  let read_string(std::istream& port)
  {
    switch (auto c { port.narrow(port.get(), '\0') }; c)
    {
    case '"': // Right Double Quotation
      return unit;

    case '\\': // Escape Sequences
      switch (auto c { port.narrow(port.get(), '\0') }; c)
      {
      case 'a':  return make<string>(make<character>('\a'), read_string(port));
      case 'b':  return make<string>(make<character>('\b'), read_string(port));
      case 't':  return make<string>(make<character>('\t'), read_string(port));
      case 'n':  return make<string>(make<character>('\n'), read_string(port));
      case 'r':  return make<string>(make<character>('\r'), read_string(port));
      case '"':  return make<string>(make<character>('"'),  read_string(port));
      case '\\': return make<string>(make<character>('\\'), read_string(port));
      case '|':  return make<string>(make<character>('|'),  read_string(port));

      case '\r':
      case '\n':
        while (is_intraline_whitespace(port.peek()))
        {
          port.ignore(1);
        }
        return read_string(port);

      default:
        return make<string>(make<character>(c), read_string(port));
      }

    default:
      return make<string>(make<character>(c), read_string(port));
    }
  }

  let make_string(const std::string& code)
  {
    std::stringstream port {};
    port << code << "\"";
    return read_string(port);
  }

  inline namespace lexical_structure
  {
    template <> auto digit< 2>() -> std::string { return "[01]"; }
    template <> auto digit< 8>() -> std::string { return "[01234567]"; }
    template <> auto digit<10>() -> std::string { return "\\d"; }
    template <> auto digit<16>() -> std::string { return "[" + digit<10>() + "abcdef]"; }

    template <> auto radix< 2>() -> std::string { return  "#b";   }
    template <> auto radix< 8>() -> std::string { return  "#o";   }
    template <> auto radix<10>() -> std::string { return "(#d)?"; }
    template <> auto radix<16>() -> std::string { return  "#x";   }

    auto exactness() -> std::string
    {
      return "(#e|#i)?";
    }

    auto sign() -> std::string
    {
      return "[\\+-]?";
    }

    auto infnan() -> std::string
    {
      return "[\\+-](inf|nan)\\.0";
    }

    auto suffix() -> std::string
    {
      return "(e" + sign() + digits<10>("+") + ")?";
    }
  }
} // namespace kernel
} // namespace meevax
