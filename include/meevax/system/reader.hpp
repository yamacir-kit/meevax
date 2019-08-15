#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <limits> // std::numeric_limits<std::streamsize>

#include <meevax/system/boolean.hpp>
#include <meevax/system/exception.hpp>
#include <meevax/system/file.hpp>
#include <meevax/system/numerical.hpp>
#include <meevax/system/string.hpp>
#include <meevax/system/symbol.hpp>

namespace meevax::system
{
  /**
   * Reader is character oriented state machine provides "read" primitive.
   *
   * This type requires the type manages symbol table as template parameter.
   */
  template <typename Environment>
  class reader
    : public input_file
  {
    using seeker = std::istream_iterator<input_file::char_type>;

    static inline const auto error_pair {make<read_error<category::pair>>(
      "ill-formed dot-notation"
    )};

    static inline const auto error_parentheses {make<read_error<category::parentheses>>(
      "unexpected close parentheses inserted"
    )};

  public:
    template <typename... Ts>
    constexpr reader(Ts&&... args)
      : input_file {std::forward<Ts>(args)...}
    {}

    template <typename... Ts>
    constexpr decltype(auto) intern(Ts&&... args)
    {
      return static_cast<Environment&>(*this).intern(std::forward<Ts>(args)...);
    }

    decltype(auto) ready() const noexcept
    {
      return operator bool();
    }

    object read()
    {
      std::string token {};

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
          auto expression {read()};
          putback('(');
          return cons(expression, read());
        }
        catch (const object& object)
        {
          if (object == error_parentheses)
          {
            return unit;
          }
          else if (object == error_pair)
          {
            auto expression {read()};
            ignore(std::numeric_limits<std::streamsize>::max(), ')'); // XXX DIRTY HACK
            return expression;
          }
          else
          {
            throw;
          }
        }

      case ')':
        throw error_parentheses;

      case '"':
        return string_literal();

      case '\'':
        return list(intern("quote"), read());

      case '`':
        return list(intern("quasiquote"), read());

      case ',':
        switch (peek())
        {
        case '@':
          ignore(1);
          return list(intern("unquote-splicing"), read());

        default:
          return list(intern("unquote"), read());
        }

      case '#':
        return discriminate();

      default:
        token.push_back(*head);

        if (auto c {peek()}; is_delimiter(c)) // delimiter
        {
          if (token == ".")
          {
            throw error_pair;
          }
          else try // is symbol or real
          {
            return make<real>(token);
          }
          catch (const std::runtime_error&) // means not numeric expression (XXX DIRTY HACK)
          {
            return intern(token);
          }
        }
      }

      return make<character>("end-of-file");
    }

    decltype(auto) string_literal()
    {
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

    object discriminate()
    {
      switch (peek())
      {
      case 't':
        read(); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return _true_;

      case 'f':
        read(); // XXX DIRTY HACK (IGNORE FOLLOWING CHARACTERS)
        return _false_;

      case '(':
        {
          auto evaluation_context {static_cast<Environment&>(*this)};

          auto expression {read()};
          auto executable {evaluation_context.compile(expression)};

          return evaluation_context.execute(executable);
        }
        // return static_cast<Environment&>(*this).execute(
        //          static_cast<Environment&>(*this).compile(
        //            read()
        //          )
        //        );

      default:
        return undefined;
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

