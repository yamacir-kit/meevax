#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <iostream>
#include <iterator> // std::begin, std::end
#include <limits> // std::numeric_limits<std::streamsize>
#include <string>
#include <utility>

#include <meevax/system/cursor.hpp>
#include <meevax/system/exception.hpp>
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

    template <typename CharType>
    constexpr auto is_delimiter(CharType&& c) const noexcept
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

    cursor operator()(std::istream& is) const
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
        if (auto first {(*this)(is)}; first == x0020) // termination
        {
          return unit;
        }
        else if (first == x002E) // dot-notation
        {
          auto second {(*this)(is)};

          if (auto maybe_x0020 {(*this)(is)}; maybe_x0020 != x0020)
          {
            is.ignore(std::numeric_limits<std::streamsize>::max(), ')');
            throw std::runtime_error {"cdr part of dot-notation allows only one expression"};
          }
          else
          {
            return second;
          }
        }
        else
        {
          is.putback('(');
          return cons(first, (*this)(is));
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
            return cursor::bind<string>(cursor::bind<character>('\n'), (*this)(is));

          case '\n':
            while (std::isspace(is.peek()))
            {
              is.ignore(1);
            }
            is.putback('"');
            return (*this)(is);

          case '"':
            is.putback('"');
            return cursor::bind<string>(cursor::bind<character>("\""), (*this)(is));

          default:
            is.putback('"');
            return cursor::bind<string>(cursor::bind<character>("#\\unsupported;"), (*this)(is));
          }

        default:
          is.putback('"');
          return cursor::bind<string>(cursor::bind<character>(c), (*this)(is));
        }

      case '\'':
        return list(module.as<modular>().intern("quote"), (*this)(is));

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

    cursor expand(std::istream& is) const
    {
      switch (is.peek())
      {
      case 't':
        (*this)(is); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return true_v;

      case 'f':
        (*this)(is); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return false_v;

      default:
        return undefined;
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

