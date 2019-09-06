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

      case ' ': case '\t': case '\v': case '\n':
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
          else throw;
        }

      case ')':
        throw error_parentheses;

      case '"':
        return read<string>(*this);

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

    template <typename T, REQUIRES(std::is_same<T, string>)>
    static object read(std::istream& stream)
    {
      switch (auto c {stream.narrow(stream.get(), '\0')}; c)
      {
      case '"': // termination
        // TODO return unit; が正しいのでは？
        // return make<string>(make<character>(""), unit);
        return unit;

      case '\\': // escape sequences
        switch (auto escaped {stream.narrow(stream.get(), '\0')}; escaped)
        {
        case 'n':
          return make<string>(make<character>('\n'), read<string>(stream));

        case '\n':
          discard(stream, whitespace);
          return read<string>(stream);

        case '"':
          return make<string>(make<character>("\""), read<string>(stream));

        default:
          return make<string>(make<character>("#\\unsupported;"), read<string>(stream));
        }

      default:
        return make<string>(make<character>(c), read<string>(stream));
      }
    }

    template <typename Predicate>
    static void discard(std::istream& stream, Predicate&& predicate)
    {
      while (predicate(stream.peek()))
      {
        stream.ignore(1);
      }
    }

  public:
    /*
     * 〈intraline whitespace〉=〈space or tab〉
     */
    static constexpr auto intraline_whitespace(char_type c) noexcept
    {
      return std::isspace(c);
    }

    /*
     * 〈whitespace〉=〈intraline whitespace〉|〈line ending〉
     */
    static constexpr auto whitespace(char_type c) noexcept
    {
      return intraline_whitespace(c) or c == u8'\r' or c == u8'\n';
    }

    template <typename CharType>
    bool is_delimiter(CharType&& c) const noexcept
    {
      switch (c)
      {
      // intraline whitespace
      case u8' ':
      case u8'\t': case u8'\v':

      // line ending
      case u8'\r': case u8'\n':

      case u8'(': case u8')':

      case u8'#':

      // quotation
      case u8'\'':
      case u8',':
      case u8'`':

      case u8'"':
      case u8';':
      case u8'|':
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
        read();
        return true_object;

      case 'f':
        read();
        return false_object;

      case '(':
        {
          auto environment {static_cast<Environment&>(*this)};

          auto expression {read()};
          auto executable {environment.compile(expression)};

          return environment.execute(executable);
        }

      case ';':
        ignore(1);
        return read(), read();

      default:
        return undefined;
      }
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

