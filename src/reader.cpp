#include <meevax/kernel/reader.hpp>
#include <meevax/posix/vt102.hpp>

namespace meevax { inline namespace kernel
{
  let const eof_object = make<eof>();

  auto operator <<(std::ostream& port, const eof&) -> decltype(port)
  {
    return port << magenta << "#,(" << green << "eof-object" << magenta << ")" << reset;
  }

  let const eos_object = make<eos>();

  auto operator <<(std::ostream& port, const eos&) -> decltype(port)
  {
    return port << magenta << "#,(" << green << "eos-object" << magenta << ")" << reset;
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
      case 'a':
        return make<string>(make<character>('\a'), read_string(port));

      case 'b':
        return make<string>(make<character>('\b'), read_string(port));

      case 'n':
        return make<string>(make<character>('\n'), read_string(port));

      case 'r':
        return make<string>(make<character>('\r'), read_string(port));

      case 't':
        return make<string>(make<character>('\t'), read_string(port));

      case '|':
        return make<string>(make<character>('|'), read_string(port));

      case '"':
        return make<string>(make<character>('"'), read_string(port));

      case '\\':
        return make<string>(make<character>('\\'), read_string(port));

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
}} // namespace meevax::kernel

