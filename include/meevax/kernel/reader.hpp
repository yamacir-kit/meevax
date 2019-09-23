#ifndef INCLUDED_MEEVAX_KERNEL_READER_HPP
#define INCLUDED_MEEVAX_KERNEL_READER_HPP

#include <istream>
#include <limits> // std::numeric_limits<std::streamsize>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/file.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax::kernel
{
  inline namespace lexical_structure
  {
    /*
     * <intraline whitespace> = <space or tab>
     */
    static constexpr auto intraline_whitespace(std::istream::char_type c)
    {
      return std::isspace(c);
    }

    static constexpr auto line_ending(std::istream::char_type c)
    {
      return c == u8'\r' or c == u8'\n';
    }

    /*
     * <whitespace> = <intraline whitespace> | <line ending>
     */
    static constexpr auto whitespace(std::istream::char_type c)
    {
      return intraline_whitespace(c) or line_ending(c);
    }

    static constexpr bool is_delimiter(std::istream::char_type c)
    {
      // return whitespace(c) or ;

      if (whitespace(c))
      {
        return true;
      }
      else switch (c)
      {
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
  } // inline namespace lexical_structure

  namespace
  {
    template <typename T>
    const object datum(std::istream& stream); // TODO Rename to simple_datum

    /*
     * <string> = " <string element> * "
     *
     * <string element> = <any character other than " or \>
     *                  | <mnemonic escape>
     *                  | \"
     *                  | \\
     *                  | \ <intraline whitespace>* <line ending> <intraline whitespace>
     *                  | <inline hex escape>
     */
    template <>
    const object datum<string>(std::istream& stream)
    {
      switch (auto c {stream.narrow(stream.get(), '\0')}; c)
      {
      case '"': // termination
        // return make<string>(make<character>(""), unit);
        return unit;

      case '\\': // escape sequences
        switch (auto escaped {stream.narrow(stream.get(), '\0')}; escaped)
        {
        case 'n':
          return make<string>(make<character>('\n'), datum<string>(stream));

        case '\n':
          while (whitespace(stream.peek()))
          {
            stream.ignore(1);
          }
          return datum<string>(stream);

        case '"':
          return make<string>(make<character>("\""), datum<string>(stream));

        default:
          return make<string>(make<character>("#\\unsupported;"), datum<string>(stream));
        }

      default:
        return make<string>(make<character>(c), datum<string>(stream));
      }
    }

    /*
     * Helper function to construct a datum from std::string. Note that it will
     * not work properly for ill-formed input.
     */
    template <typename T>
    decltype(auto) datum(const std::string& expression)
    {
      return datum<T>(std::istringstream {expression});
    }

    template <>
    decltype(auto) datum<string>(const std::string& expression)
    {
      std::istringstream stream {expression + "\""};
      return datum<string>(stream);
    }
  }

  /*
   * Reader is character oriented state machine provides "read" primitive. This
   * type requires the type manages symbol table as template parameter.
   */
  template <typename Environment>
  class reader
    : public input_file
  {
    using seeker = std::istream_iterator<input_file::char_type>;

    // static inline const auto error_pair {make<read_error<category::pair>>(
    //   "ill-formed dot-notation"
    // )};
    //
    // static inline const auto error_parentheses {make<read_error<category::parentheses>>(
    //   "unexpected close parentheses inserted"
    // )};

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

    decltype(auto) read() noexcept(false)
    {
      return read(*this);
    }

    decltype(auto) read(const std::string& expression) noexcept(false)
    {
      std::stringstream stream {expression};
      return read(stream);
    }

    /*
     * The external representation.
     *
     * <Datum> is what the read procedure successfully parses. Note that any
     * string that parses as an <expression> will also parse as a <datum>.
     *
     * <datum> = <simple datum>
     *         | <compund datum>
     *
     * <simple datum> = <boolean>
     *                | <number>
     *                | <character>
     *                | <string>
     *                | <symbol>
     *
     * <compound datum> = <list>
     *                  | <abbreviation>
     *                  | <read time evaluation>
     *
     * <list> = (<datum>*)
     *        | (<datum>+ . <datum>)
     *
     * <abbreviation> = <abbrev prefix> <datum>
     *
     * <abbrev prefix> = ' | ` | , | ,@
     *
     * <read time evaluation> = #(<datum>*)
     *
     * <label> = # <unsigned integer 10>                           unimplemented
     */
    const object read(std::istream& stream)
    {
      std::string token {};

      for (seeker head {stream}; head != seeker {}; ++head) switch (*head)
      {
      case ';':
        stream.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        break;

      case ' ': case '\t': case '\v': case '\n':
        break;

      case '(':
        try
        {
          auto expression {read(stream)};
          stream.putback('(');
          return cons(expression, read(stream));
        }
        catch (const reader_error_about_parentheses& error)
        {
          return unit;
        }
        catch (const reader_error_about_pair& error)
        {
          auto expression {read(stream)};
          stream.ignore(std::numeric_limits<std::streamsize>::max(), ')'); // XXX DIRTY HACK
          return expression;
        }

        // catch (const object& object)
        // {
        //   if (object == error_parentheses)
        //   {
        //     return unit;
        //   }
        //   else if (object == error_pair)
        //   {
        //     auto expression {read(stream)};
        //     stream.ignore(std::numeric_limits<std::streamsize>::max(), ')'); // XXX DIRTY HACK
        //     return expression;
        //   }
        //   else throw;
        // }

      case ')':
        throw reader_error_about_parentheses {
          "unexpected close parentheses inserted"
        };

      case '"':
        return datum<string>(stream);

      case '\'':
        return list(intern("quote"), read(stream));

      case '`':
        return list(intern("quasiquote"), read(stream));

      case ',':
        switch (stream.peek())
        {
        case '@':
          stream.ignore(1);
          return list(intern("unquote-splicing"), read(stream));

        default:
          return list(intern("unquote"), read(stream));
        }

      case '#':
        return discriminate(stream);

      default:
        token.push_back(*head);

        if (auto c {stream.peek()}; is_delimiter(c)) // delimiter
        {
          if (token == ".")
          {
            throw reader_error_about_pair {
              "dot-notation"
            };
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

      return characters.at("end-of-file");
    }

    object discriminate(std::istream& stream)
    {
      switch (stream.peek())
      {
      case 'f':
        read(stream);
        return false_object;

      case 't':
        read(stream);
        return true_object;

      /*
       * Read-time-evaluation #( ... )
       */
      case '(':
        return static_cast<Environment&>(*this).evaluate(read(stream));

      case '\\':
        {
          stream.get();

          std::string name {};

          for (auto c {stream.peek()}; not is_delimiter(c); c = stream.peek())
          {
            name.push_back(stream.get());
          }

          // TODO Provide user-defined character-name?
          static const std::unordered_map<std::string, std::string> alias
          {
            {"alarm", "bell"}, // for R7RS
            {"newline", "line-feed"}, // for R7RS
            {"return", "carriage-return"}, // for R7RS
            {"tab", "horizontal-tabulation"}, // for R7RS
          };

          if (auto iter {alias.find(name)}; iter != std::end(alias))
          {
            name = std::get<1>(*iter);
          }

          // TODO Provide datum<character>(name)?
          if (auto iter {characters.find(name)}; iter != std::end(characters))
          {
            return std::get<1>(*iter);
          }
          else
          {
            throw reader_error_about_character {name, " is unknown character-name"};
          }
        }

      case ';':
        stream.ignore(1);
        return read(stream), read(stream);

      default:
        return undefined;
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP

