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

  /* ---- R7RS 7.1.1. Lexical structure ----------------------------------------
   *
   *  <character> = #\ <any character>
   *              | #\ <character name>
   *              | #\x <hex scalar value>
   *
   *  <character name> = alarm
   *                   | backspace
   *                   | delete
   *                   | escape
   *                   | newline
   *                   | null
   *                   | return
   *                   | space
   *                   | tab
   *
   * ------------------------------------------------------------------------ */
  let read_char(input_port & port)
  {
    auto const token = read_token(port);

    std::unordered_map<bytestring, char> static const names
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

    if (auto const iter = names.find(token); iter != std::end(names))
    {
      return make<character>(std::get<1>(*iter));
    }
    else
    {
      return make<character>(token);
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
} // namespace kernel
} // namespace meevax
