#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <fstream>
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
    : public std::ifstream
  {
    const cursor x0020, x002E;

  public:
    template <typename... Ts>
    reader(Ts&&... args)
      : std::ifstream {std::forward<Ts>(args)...}
      , x0020 {cursor::bind<std::string>("#\\x0020")}
      , x002E {cursor::bind<std::string>("#\\x002E")}
    {}

    cursor read(modular& module)
    {
      for (std::string buffer {narrow(get(), ' ')}; *this; buffer.push_back(narrow(get(), ' '))) switch (buffer.back())
      {
      case ';':
        ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        [[fallthrough]];

      case ' ': case '\t': case '\n':
        buffer.pop_back();
        break;

      case '(':
        if (auto first {read(module)}; first == x0020) // termination
        {
          return unit;
        }
        else if (first == x002E) // dot-notation
        {
          auto second {read(module)};
          ignore(std::numeric_limits<std::streamsize>::max(), ')');
          return second;
        }
        else
        {
          putback('(');
          return cons(first, read(module));
        }

      case ')':
        return x0020;

      case '"':
        switch (auto c {narrow(get(), '\0')}; c)
        {
        case '"': // termination
          return cursor::bind<string>(cursor::bind<character>(""), unit);

        case '\\': // escape sequences
          switch (auto escaped {narrow(get(), '\0')}; escaped)
          {
          case 'n':
            putback('"');
            return cursor::bind<string>(cursor::bind<character>('\n'), read(module));

          case '\n':
            while (std::isspace(peek()))
            {
              ignore(1);
            }
            putback('"');
            return read(module);

          case '"':
            putback('"');
            return cursor::bind<string>(cursor::bind<character>("\""), read(module));

          default:
            putback('"');
            return cursor::bind<string>(cursor::bind<character>("#\\unsupported;"), read(module));
          }

        default:
          putback('"');
          return cursor::bind<string>(cursor::bind<character>(c), read(module));
        }

      case '\'':
        return list(module.intern("quote"), read(module));

      case '#':
        return expand(module);

      default:
        if (auto c {peek()}; is_delimiter(c)) try // delimiter
        {
          if (buffer == ".")
          {
            return x002E;
          }
          else
          {
            return cursor::bind<number>(buffer);
          }
        }
        catch (const std::runtime_error&)
        {
          return module.intern(buffer);
        }
      }

      return cursor::bind<character>("end-of-file");
    }

    template <typename... Ts>
    decltype(auto) operator()(Ts&&... args)
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

    cursor expand(modular& module)
    {
      switch (peek())
      {
      case 't':
        read(module); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return true_v;

      case 'f':
        read(module); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return false_v;

      default:
        return undefined;
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

