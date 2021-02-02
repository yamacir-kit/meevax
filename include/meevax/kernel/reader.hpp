#ifndef INCLUDED_MEEVAX_KERNEL_READER_HPP
#define INCLUDED_MEEVAX_KERNEL_READER_HPP

#include <limits> // std::numeric_limits<std::streamsize>
#include <regex>
#include <sstream>
#include <stack>

#include <boost/iostreams/device/null.hpp>
#include <boost/iostreams/stream_buffer.hpp>

#include <meevax/iostream/ignore.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/miscellaneous.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/parser.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/vector.hpp>
#include <meevax/string/header.hpp>

namespace meevax
{
inline namespace kernel
{
  auto read_token(input_port & port) -> bytestring;

  let read_char(input_port &);

  /* ---- String Constructor ---------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */

  let read_string(std::istream& port);

  let make_string(bytestring const&);

  /* ---- Number Constructor ---------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */

  inline namespace lexical_structure
  {
    template <std::size_t R>
    auto digit() -> bytestring;

    template <> auto digit< 2>() -> bytestring;
    template <> auto digit< 8>() -> bytestring;
    template <> auto digit<10>() -> bytestring;
    template <> auto digit<16>() -> bytestring;

    template <std::size_t R>
    auto digits(bytestring const& quantifier)
    {
      return digit<R>() + quantifier;
    }

    template <std::size_t R>
    auto radix() -> bytestring;

    template <> auto radix< 2>() -> bytestring;
    template <> auto radix< 8>() -> bytestring;
    template <> auto radix<10>() -> bytestring;
    template <> auto radix<16>() -> bytestring;

    auto exactness() -> bytestring;

    auto sign() -> bytestring;

    auto infnan() -> bytestring;

    auto suffix() -> bytestring;

    template <std::size_t R = 10>
    auto prefix() -> const bytestring
    {
      return "(" + radix<R>() + exactness() + "|" + exactness() + radix<R>() + ")";
    }

    template <std::size_t R = 10>
    auto unsigned_integer() -> const bytestring
    {
      return digits<R>("+");
    }

    template <std::size_t R = 10>
    auto signed_integer() -> const bytestring
    {
      return sign() + unsigned_integer<R>();
    }

    template <std::size_t R = 10>
    auto decimal() -> const bytestring
    {
      return "(" + unsigned_integer<R>()                   + suffix() +
             "|"                    "\\." + digits<R>("+") + suffix() +
             "|" + digits<R>("+") + "\\." + digits<R>("*") + suffix() + ")";
    }

    template <std::size_t R>
    auto unsigned_real() -> const bytestring
    {
      return "("  + unsigned_integer<R>()                                 +
             "|(" + unsigned_integer<R>() + ")/(" + unsigned_integer<R>() + ")" +
             "|"  +          decimal<R>()                                 + ")";
    }

    template <std::size_t R = 10>
    auto signed_real() -> const bytestring
    {
      return "(" + sign() + unsigned_real<R>() + "|" + infnan() + ")";
    }

    template <std::size_t R = 10>
    auto signed_complex() -> const bytestring
    {
      return "(" "(" + signed_real<R>() +                                       ")"
             "|" "(" + signed_real<R>() +       "@" +   signed_real<R>() +      ")"
             "|" "(" + signed_real<R>() + "([\\+-]" + unsigned_real<R>() + ")i" ")"
             "|" "(" + signed_real<R>() + "([\\+-]" +                      ")i" ")"
             "|" "(" + signed_real<R>() + "("       +           infnan() + ")i" ")"
             "|" "(" +                    "([\\+-]" + unsigned_real<R>() + ")i" ")"
             "|"     +                    "("       +           infnan() + ")i"
             "|"     +                    "([\\+-]"                        ")i"
             ")";
    }

    template <std::size_t R = 10>
    auto number() -> const bytestring
    {
      return prefix<R>() + signed_complex<R>();
    }
  } // inline namespace lexical_structure

  template <std::size_t R = 10>
  auto is_number(bytestring const& token)
  {
    static const std::regex pattern { number<R>() };
    std::smatch result {};
    return std::regex_match(token, result, pattern);
  }

  template <std::size_t R = 10>
  let make_number(bytestring const& token)
  {
    static const std::unordered_map<bytestring, object> srfi_144
    {
      std::make_pair("fl-pi", make<default_float>(boost::math::constants::pi<default_float::value_type>())),
    };

    static const std::regex pattern { number<R>() };

    if (const auto iter { srfi_144.find(token) }; iter != std::end(srfi_144))
    {
      return cdr(*iter);
    }
    else if (std::smatch result {}; std::regex_match(token, result, pattern))
    {
      // FOR DEVELOPER
      // for (auto iter { std::begin(result) }; iter != std::end(result); ++iter)
      // {
      //   if ((*iter).length())
      //   {
      //     switch (auto index { std::distance(std::begin(result), iter) }; index)
      //     {
      //     default:
      //       std::cout << "; number[" << index << "/" << result.size() << "] = " << *iter << std::endl;
      //       break;
      //     }
      //   }
      // }

      // if (result.length(30)) // 6, 30, 31, 32, 38, 39
      // {
      //   return make<complex>(make_number<R>(result.str(31)), make_number<R>(result.str(38)));
      // }
      //
      // if (result.length(53)) // 6, 53, 54, 55, 61, 62
      // {
      //   return make<complex>(make_number<R>(result.str(54)), make_number<R>(result.str(61)));
      // }

      if (result.length(16)) // 6, 7, 8, 16
      {
        static const std::unordered_map<bytestring, object> infnan
        {
          std::make_pair("+inf.0", make<default_float>(+default_float::infinity())),
          std::make_pair("-inf.0", make<default_float>(-default_float::infinity())),
          std::make_pair("+nan.0", make<default_float>(+default_float::quiet_NaN())),
          std::make_pair("-nan.0", make<default_float>(-default_float::quiet_NaN()))
        };

        return infnan.at(token);
      }

      if (result.length(12)) // 6, 7, 8, 9, 12
      {
        return make<default_float>(token.substr(token[0] == '+' ? 1 : 0));
      }

      if (result.length(10) and result.length(11)) // 6, 7, 8, 9, 10, 11
      {
        auto const value {
          ratio(make_number<R>(result.str(10)),
                make_number<R>(result.str(11))).reduce() };

        if (value.is_integer())
        {
          return car(value);
        }
        else
        {
          return make(value);
        }
      }

      if (result.length(9)) // 6, 7, 8, 9
      {
        return make<exact_integer>(token.substr(token[0] == '+' ? 1 : 0));
      }

      throw read_error<void>("the given token ", token, " is a valid Scheme numeric literal, but is not yet supported");
    }
    else
    {
      throw read_error<void>("the given token ", token, " is a invalid Scheme numeric literal");
    }
  }

  /* ---- Reader ---------------------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */
  template <typename SK>
  class reader
  {
    friend SK;

    explicit reader()
    {}

    IMPORT(SK, evaluate,);
    IMPORT(SK, intern,);
    IMPORT(SK, standard_debug_port,);
    IMPORT(SK, write_to,);

    using seeker = std::istream_iterator<std::istream::char_type>;

    enum class   proper_list_tag {};
    enum class improper_list_tag {};

  public:
    /* ---- Read ---------------------------------------------------------------
     *
     *  TODO
     *    Rename read(std::istream&) => read_from
     *
     * ---------------------------------------------------------------------- */
    let const read(std::istream& port)
    {
      bytestring token {};

      for (seeker head { port }; head != seeker {}; ++head) switch (*head)
      {
      case ';':
        port.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        break;

      case ' ': case '\f': case '\n': case '\r': case '\t': case '\v':
        break;

      case '(':
        try
        {
          auto expression {read(port)};
          port.putback('(');
          return cons(expression, read(port));
        }
        catch (read_error<proper_list_tag> const&)
        {
          return unit;
        }
        catch (read_error<improper_list_tag> const&)
        {
          let y = read(port);
          port.ignore(std::numeric_limits<std::streamsize>::max(), ')'); // XXX DIRTY HACK
          return y;
        }

      case ')':
        throw read_error<proper_list_tag>("unexpected close parentheses inserted");

      case '#':
        return discriminate(port);

      case '"':
        return read_string(port);

      case '\'':
        return list(intern("quote"), read(port));

      case '`':
        return list(intern("quasiquote"), read(port));

      case ',':
        switch (port.peek())
        {
        case '@':
          port.ignore(1);
          return list(intern("unquote-splicing"), read(port));

        default:
          return list(intern("unquote"), read(port));
        }

      default:
        token.push_back(*head);

        if (auto c { port.peek() }; is_end_of_token(c))
        {
          if (token == ".")
          {
            throw read_error<improper_list_tag>("dot-notation");
          }
          else try
          {
            return make_number(token);
          }
          catch (...)
          {
            return intern(token);
          }
        }
      }

      return eof_object;
    }

    auto read(object const& x) -> decltype(auto)
    {
      if (x.is_polymorphically<input_port>())
      {
        return read(x.as<input_port>());
      }
      else
      {
        throw read_error<void>(__FILE__, ":", __LINE__);
      }
    }

    let const read()
    {
      let const result = read(standard_input_port());

      write_to(standard_debug_port(), "\n", header(__func__), result, "\n");

      return result;
    }

    auto read(const bytestring& s) -> decltype(auto)
    {
      std::stringstream ss { s };
      return read(ss);
    }

  public:
    let standard_input_port() const noexcept
    {
      let static port = make<standard_input>();
      return port;
    }

    auto ready() // TODO RENAME TO 'char-ready'
    {
      return not standard_input_port().template is<null>() and standard_input_port().template as<input_port>();
    }

  private:
    let discriminate(input_port & is)
    {
      switch (is.peek())
      {
      case '!':
        is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        return read(is);

      case ',': // SRFI-10
        is.ignore(1);
        return evaluate(read(is));

      case ';': // SRFI-62
        is.ignore(1);
        return read(is), read(is);

      case 'c': // Common Lisp
        is.ignore(1);

        if (let const xs { read(is) }; xs.is<null>() or not xs.is<pair>())
        {
          return make<complex>(make<exact_integer>(0), make<exact_integer>(0));
        }
        else if (cdr(xs).is<null>() or not cdr(xs).is<pair>())
        {
          return make<complex>(car(xs), make<exact_integer>(0));
        }
        else
        {
          return make<complex>(car(xs), cadr(xs));
        }

      case 'd':
        is.ignore(1);
        return make<exact_integer>(read_token(is));

      case 'f':
        ignore(is, [](auto&& x) { return not is_end_of_token(x); });
        return f;

      case 'o':
        is.ignore(1);
        return make<exact_integer>("0" + read_token(is));

      case 'p':
        is.ignore(1);

        switch (is.peek())
        {
        case '"':
          is.ignore(1);
          return make<path>(read_string(is).as<string>());

        default:
          return make<path>(read_token(is));
        }

      case 't':
        ignore(is, [](auto&& x) { return not is_end_of_token(x); });
        return t;

      case 'x':
        is.ignore(1);
        return make<exact_integer>("0x" + read_token(is));

      case '(':
        return make<vector>(for_each_in, read(is));

      case '\\':
        is.ignore(1);
        return read_char(is);

      default:
        is.ignore(1);
        return unspecified; // XXX
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP
