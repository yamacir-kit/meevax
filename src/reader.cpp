#include <meevax/kernel/reader.hpp>

namespace meevax
{
inline namespace kernel
{
  auto read_token(input_port & port) -> std::string
  {
    std::string token {};

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
    auto any_character = [&](auto const& token, auto)
    {
      switch (token.size())
      {
      case 0:
        return make<character>(port.get());

      case 1:
        return make<character>(token.front());

      default:
        throw read_error<character>(
          "If <character> in #\\<character> is alphabetic, then any character "
          "immediately following <character> cannot be one that can appear in "
          "an identifier");
      }
    };

    auto character_name = [](auto const& token, auto)
    {
      std::unordered_map<std::string, char> static const names
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

      return make<character>(names.at(token));
    };

    auto hex_scalar_value = [](auto const& token, auto = 16)
    {
      if (token.front() == 'x' and 1 < token.size())
      {
        std::stringstream ss;
        ss << std::hex << token.substr(1);

        codepoint value = 0;
        ss >> value;

        return make<character>(value);
      }
      else
      {
        throw read_error<character>("invalid character literal: #\\", token);
      }
    };

    auto to_character = hex_scalar_value | character_name | any_character;

    return to_character(read_token(port), 16);
  }

  let read_string(input_port & port)
  {
    switch (auto c = port.narrow(port.get(), '\0'); c)
    {
    case '"': // Right Double Quotation
      return unit;

    case '\\': // Escape Sequences
      switch (auto c = port.narrow(port.get(), '\0'); c)
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

  let make_string(std::string const& text)
  {
    std::stringstream port {};
    port << text << "\"";
    return read_string(port);
  }
} // namespace kernel
} // namespace meevax
