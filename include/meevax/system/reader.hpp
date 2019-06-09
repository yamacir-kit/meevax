#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <fstream>
#include <limits> // std::numeric_limits<std::streamsize>

#include <meevax/system/boolean.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/number.hpp>
#include <meevax/system/string.hpp>
#include <meevax/system/symbol.hpp>

// TODO ポート型として標準ストリームを使うこと（リーダはストリームアダプタ）

namespace meevax::system
{
  /**
   * Reader is character oriented state machine provides "read" primitive.
   *
   * This type requires the type manages symbol table as template parameter.
   */
  template <typename Module>
  class reader
    : public std::ifstream
  {
    using seeker = std::istream_iterator<std::ifstream::char_type>;

    enum class category
    {
      pair, parentheses
    };

    template <category>
    struct ill_formed_expression
      : public exception
    {
      template <typename... Ts>
      constexpr ill_formed_expression(Ts&&... args)
        : exception {std::forward<Ts>(args)...}
      {}
    };

  public:
    template <typename... Ts>
    constexpr reader(Ts&&... args)
      : std::ifstream {std::forward<Ts>(args)...}
    {}

    template <typename... Ts>
    constexpr decltype(auto) intern(Ts&&... args)
    {
      return static_cast<Module&>(*this).intern(std::forward<Ts>(args)...);
    }

    object read()
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
          auto buffer {read()};
          putback('(');
          return cons(buffer, read());
        }
        catch (const ill_formed_expression<category::parentheses>&)
        {
          return unit;
        }
        catch (const ill_formed_expression<category::pair>&)
        {
          auto buffer {read()};
          ignore(std::numeric_limits<std::streamsize>::max(), ')'); // XXX DIRTY HACK
          return buffer;
        }

      case ')':
        throw ill_formed_expression<category::parentheses> {"unexpected close parentheses inserted"};

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
            return make<string>(make<character>('\n'), read());

          case '\n':
            while (std::isspace(peek()))
            {
              ignore(1);
            }
            putback('"');
            return read();

          case '"':
            putback('"');
            return make<string>(make<character>("\""), read());

          default:
            putback('"');
            return make<string>(make<character>("#\\unsupported;"), read());
          }

        default:
          putback('"');
          return make<string>(make<character>(c), read());
        }

      case '\'':
        return list(intern("quote"), read());

      case '`':
        return list(intern("quasiquote"), read());

      case ',':
        if (peek() != '@')
        {
          return list(intern("unquote"), read());
        }
        else
        {
          get();
          return list(intern("unquote-splicing"), read());
        }

      case '#':
        return expand();

      default:
        buffer.push_back(*head);

        if (auto c {peek()}; is_delimiter(c)) // delimiter
        {
          if (buffer == ".")
          {
            throw ill_formed_expression<category::pair> {"ill-formed dot-notation detected"};
          }
          else try // is symbol or number
          {
            return make<number>(buffer);
          }
          catch (const std::runtime_error&) // means not numeric expression (XXX DIRTY HACK)
          {
            return intern(buffer);
          }
        }
      }

      return make<character>("end-of-file");
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

    object expand()
    {
      switch (peek())
      {
      case 't':
        read(); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return _true_;

      case 'f':
        read(); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return _false_;

      default:
        return undefined;
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

