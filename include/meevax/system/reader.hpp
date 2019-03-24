#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <iostream>
#include <iterator> // std::begin, std::end
#include <limits> // std::numeric_limits<std::streamsize>
#include <string>
#include <utility> // std::forward

#include <meevax/system/cursor.hpp>
#include <meevax/system/modular.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/string.hpp>

namespace meevax::system
{
  class reader
  {
    const cursor module;
    const cursor x0020, x002E;

  public:
    explicit reader(const cursor& module)
      : module {module}
      , x0020 {cursor::bind<std::string>("#\\x0020")}
      , x002E {cursor::bind<std::string>("#\\x002E")}
    {}

    cursor read(std::istream& is) const
    {
      for (std::string buffer {is.narrow(is.get(), ' ')}; is; buffer.push_back(is.narrow(is.get(), ' '))) switch (buffer.back())
      {
      case ';':
        is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        [[fallthrough]];

      case ' ': case '\t': case '\n':
        buffer.pop_back();
        break;

      case '(':
        if (auto first {read(is)}; first == x0020) // termination
        {
          return unit;
        }
        else if (first == x002E) // dot-notation
        {
          auto second {read(is)};
          is.ignore(std::numeric_limits<std::streamsize>::max(), ')');
          return second;
        }
        else
        {
          is.putback('(');
          return cons(first, read(is));
        }

      case ')':
        return x0020;

      case '"':
        switch (auto c {is.narrow(is.get(), '\0')}; c)
        {
        case '"': // termination
          return cursor::bind<string>(cursor::bind<character>(""), unit);

        case '\\': // escape sequences
          switch (auto escaped {is.narrow(is.get(), '\0')}; escaped)
          {
          case 'n':
            is.putback('"');
            return cursor::bind<string>(cursor::bind<character>('\n'), read(is));

          case '\n':
            while (std::isspace(is.peek()))
            {
              is.ignore(1);
            }
            is.putback('"');
            return read(is);

          case '"':
            is.putback('"');
            return cursor::bind<string>(cursor::bind<character>("\""), read(is));

          default:
            is.putback('"');
            return cursor::bind<string>(cursor::bind<character>("#\\unsupported;"), read(is));
          }

        default:
          is.putback('"');
          return cursor::bind<string>(cursor::bind<character>(c), read(is));
        }

      case '\'':
        return list(module.as<modular>().intern("quote"), read(is));

      case '#':
        return expand(is);

      case '.': // XXX UGLY CODE
        if (is.peek() != '.' && buffer == ".")
        {
          return x002E;
        }
        [[fallthrough]];

      default:
        if (auto c {is.peek()}; is_delimiter(c)) try // delimiter
        {
          return cursor::bind<number>(buffer);
        }
        catch (const std::runtime_error&)
        {
          return module.as<modular>().intern(buffer);
        }
      }

      return cursor::bind<character>("end-of-file");
    }

    template <typename... Ts>
    decltype(auto) operator()(Ts&&... args) const
    {
      return read(std::forward<Ts>(args)...);
    }

  protected:
    template <typename CharType>
    constexpr bool is_delimiter(CharType&& c) const noexcept
    {
      switch (c)
      {
      case u8'\x09': // '\t':
      case u8'\x0A': // '\n':
      case u8'\x0D': // '\r':
      case u8'\x20': // ' ':
      case u8'\x22': // '"':
      case u8'\x23': // '#':
      case u8'\x27': // '\'':
      case u8'\x28': // '(':
      case u8'\x29': // ')':
      case u8'\x2C': // ',':
      case u8'\x3B': // ';':
      case u8'\x60': // '`':
      case u8'\x7C': // '|':
        return true;

      default:
        return false;
      }
    }

    cursor expand(std::istream& is) const
    {
      switch (is.peek())
      {
      case 't':
        read(is); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return true_v;

      case 'f':
        read(is); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return false_v;

      default:
        return undefined;
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

