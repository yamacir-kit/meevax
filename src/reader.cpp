#include <meevax/kernel/reader.hpp>

namespace meevax
{
inline namespace kernel
{
  auto read_token(input_port & port) -> bytestring
  {
    bytestring token {};

    for (auto c = port.peek(); not is_end_of_token(c); c = port.peek())
    {
      token.push_back(port.get());
    }

    return token;
  }

  let read_char(input_port & port)
  {
    auto name { read_token(port) };

    if (name.empty())
    {
      name.push_back(port.get());
    }

    static const std::unordered_map<bytestring, char> names
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

  let make_string(bytestring const& text)
  {
    std::stringstream port {};
    port << text << "\"";
    return read_string(port);
  }

  inline namespace lexical_structure
  {
    template <> auto digit< 2>() -> bytestring { return "[01]"; }
    template <> auto digit< 8>() -> bytestring { return "[01234567]"; }
    template <> auto digit<10>() -> bytestring { return "\\d"; }
    template <> auto digit<16>() -> bytestring { return "[" + digit<10>() + "abcdef]"; }

    template <> auto radix< 2>() -> bytestring { return  "#b";   }
    template <> auto radix< 8>() -> bytestring { return  "#o";   }
    template <> auto radix<10>() -> bytestring { return "(#d)?"; }
    template <> auto radix<16>() -> bytestring { return  "#x";   }

    auto exactness() -> bytestring
    {
      return "(#e|#i)?";
    }

    auto sign() -> bytestring
    {
      return "[\\+-]?";
    }

    auto infnan() -> bytestring
    {
      return "[\\+-](inf|nan)\\.0";
    }

    auto suffix() -> bytestring
    {
      return "(e" + sign() + digits<10>("+") + ")?";
    }
  }
} // namespace kernel
} // namespace meevax
