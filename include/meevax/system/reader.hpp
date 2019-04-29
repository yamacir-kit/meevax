#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <fstream>
#include <limits> // std::numeric_limits<std::streamsize>

#include <meevax/system/boolean.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/string.hpp>
#include <meevax/system/symbol.hpp>

namespace meevax::system
{
  class reader // is character oriented state machine.
    : public std::ifstream
  {
    using seeker = std::istream_iterator<char8_t>;

    struct dot_notation // XXX TEMPORARY
      : public exception
    {
      template <typename... Ts>
      constexpr dot_notation(Ts&&... args)
        : exception {std::forward<Ts>(args)...}
      {}
    };

    struct list_termination // XXX TEMPORARY
      : public exception
    {
      template <typename... Ts>
      constexpr list_termination(Ts&&... args)
        : exception {std::forward<Ts>(args)...}
      {}
    };

  public:
    template <typename... Ts>
    constexpr reader(Ts&&... args)
      : std::ifstream {std::forward<Ts>(args)...}
    {}

    template <typename Interner>
    cursor read(Interner&& intern) noexcept(false)
    {
      std::string buffer {};

      for (seeker head {*this}; head != seeker {}; ++head) switch (*head)
      {
      case ';':
        ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        break;

      case ' ': case '\t': case '\n':
        break;

      case '(':
        try
        {
          auto buffer {read(intern)};
          putback('(');
          return cons(buffer, read(intern));
        }
        catch (const list_termination&)
        {
          return unit;
        }
        catch (const dot_notation&)
        {
          auto buffer {read(intern)};
          ignore(std::numeric_limits<std::streamsize>::max(), ')'); // XXX DIRTY HACK
          return buffer;
        }

      case ')':
        throw list_termination {"ill-formed-expression"};

      case '"':
        switch (auto c {narrow(get(), '\0')}; c)
        {
        case '"': // termination
          return make<string>(make<character>(""), unit);

        case '\\': // escape sequences
          switch (auto escaped {narrow(get(), '\0')}; escaped)
          {
          case 'n':
            putback('"');
            return make<string>(make<character>('\n'), read(intern));

          case '\n':
            while (std::isspace(peek()))
            {
              ignore(1);
            }
            putback('"');
            return read(intern);

          case '"':
            putback('"');
            return make<string>(make<character>("\""), read(intern));

          default:
            putback('"');
            return make<string>(make<character>("#\\unsupported;"), read(intern));
          }

        default:
          putback('"');
          return make<string>(make<character>(c), read(intern));
        }

      case '\'':
        return list(intern("quote"), read(intern));

      case '#':
        return expand(intern);

      default:
        buffer.push_back(*head);

        if (auto c {peek()}; is_delimiter(c)) try // delimiter
        {
          if (buffer == ".")
          {
            throw dot_notation {"ill-formed-expresion with dot-notation"};
          }
          else
          {
            return make<number>(buffer);
          }
        }
        catch (const std::runtime_error&)
        {
          return intern(buffer);
        }
      }

      return make<character>("end-of-file");
    }

    template <typename... Ts>
    decltype(auto) operator()(Ts&&... args)
    {
      return read(std::forward<Ts>(args)...);
    }

  protected:
    template <typename CharType>
    bool is_delimiter(CharType&& c) const noexcept
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

    template <typename Interner>
    cursor expand(Interner&& intern)
    {
      switch (peek())
      {
      case 't':
        read(intern); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return true_v;

      case 'f':
        read(intern); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return false_v;

      default:
        return undefined;
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

