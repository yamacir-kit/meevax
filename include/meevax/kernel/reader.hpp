#ifndef INCLUDED_MEEVAX_KERNEL_READER_HPP
#define INCLUDED_MEEVAX_KERNEL_READER_HPP

#include <istream>
#include <limits> // std::numeric_limits<std::streamsize>
#include <regex>
#include <stack>

#include <boost/iostreams/device/null.hpp>
#include <boost/iostreams/stream_buffer.hpp>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/path.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax { inline namespace kernel
{
  /* ---- End-of-File ------------------------------------------------------- */

  struct eof
  {};

  extern let const eof_object;

  auto operator <<(std::ostream& port, const eof&) -> decltype(port);

  /* ---- End-of-String ----------------------------------------------------- */

  struct eos
  {};

  extern let const eos_object;

  auto operator <<(std::ostream& port, const eos&) -> decltype(port);

  /* ---- String Constructor ---------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */

  let read_string(std::istream& port);

  let make_string(const std::string&);

  /* ---- Number Constructor ---------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */

  inline namespace lexical_structure
  {
    template <std::size_t R>
    auto digit() -> std::string;

    template <> auto digit< 2>() -> std::string;
    template <> auto digit< 8>() -> std::string;
    template <> auto digit<10>() -> std::string;
    template <> auto digit<16>() -> std::string;

    template <std::size_t R>
    auto digits(const std::string& quantifier)
    {
      return digit<R>() + quantifier;
    }

    template <std::size_t R>
    auto radix() -> std::string;

    template <> auto radix< 2>() -> std::string;
    template <> auto radix< 8>() -> std::string;
    template <> auto radix<10>() -> std::string;
    template <> auto radix<16>() -> std::string;

    auto exactness() -> std::string;

    auto sign() -> std::string;

    auto infnan() -> std::string;

    auto suffix() -> std::string;

    template <std::size_t R = 10>
    auto prefix() -> const std::string
    {
      return "(" + radix<R>() + exactness() + "|" + exactness() + radix<R>() + ")";
    }

    template <std::size_t R = 10>
    auto unsigned_integer() -> const std::string
    {
      return digits<R>("+");
    }

    template <std::size_t R = 10>
    auto signed_integer() -> const std::string
    {
      return sign() + unsigned_integer<R>();
    }

    template <std::size_t R = 10>
    auto decimal() -> const std::string
    {
      return "(" + unsigned_integer<R>()                   + suffix() +
             "|"                    "\\." + digits<R>("+") + suffix() +
             "|" + digits<R>("+") + "\\." + digits<R>("*") + suffix() + ")";
    }

    template <std::size_t R>
    auto unsigned_real() -> const std::string
    {
      return "("  + unsigned_integer<R>()                                 +
             "|(" + unsigned_integer<R>() + ")/(" + unsigned_integer<R>() + ")" +
             "|"  +          decimal<R>()                                 + ")";
    }

    template <std::size_t R = 10>
    auto signed_real() -> const std::string
    {
      return "(" + sign() + unsigned_real<R>() + "|" + infnan() + ")";
    }

    template <std::size_t R = 10>
    auto signed_complex() -> const std::string
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
    auto number() -> const std::string
    {
      return prefix<R>() + signed_complex<R>();
    }
  } // inline namespace lexical_structure

  template <std::size_t R = 10>
  auto is_number(const std::string& token)
  {
    static const std::regex pattern { number<R>() };
    std::smatch result {};
    return std::regex_match(token, result, pattern);
  }

  template <std::size_t R = 10>
  let make_number(const std::string& token)
  {
    static const std::unordered_map<std::string, object> srfi_144
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
        static const std::unordered_map<std::string, object> infnan
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
        ratio value {
          make_number<R>(result.str(10)),
          make_number<R>(result.str(11))
        };

        if (value.reduce().is_integer())
        {
          return value.numerator();
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

  /* ==== Reader ===============================================================
  *
  *
  * ========================================================================= */
  template <typename SK>
  class reader
    // : private boost::iostreams::stream_buffer<boost::iostreams::null_sink>
  {
    friend SK;

    explicit reader()
      // : sources {}
    {
      // sources.emplace(std::cin.rdbuf());
    }

    Import(SK, evaluate);
    Import(SK, intern);

    using seeker = std::istream_iterator<std::istream::char_type>;

    enum class   proper_list_tag {};
    enum class improper_list_tag {};

  protected:
    // NOTE
    // std::stack<object> sources {};
    // auto path {read_string("/path/to/file")};
    // auto something {make<input_port>(path.as<std::string>())};
    // car(something) = path;
    // cdr(something) = make<exact_integer>(1); // current line

    // std::stack<std::istream> sources;

  public:
    /* ---- Read ---------------------------------------------------------------
     *
     *  TODO
     *    Rename read(std::istream&) => read_from
     *
     * ---------------------------------------------------------------------- */
    let const read(std::istream& port)
    {
      std::string token {};

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
        catch (const read_error<proper_list_tag>&)
        {
          return unit;
        }
        catch (const read_error<improper_list_tag>&)
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

        if (auto c {port.peek()}; is_delimiter(c)) // delimiter
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

    auto read(std::istream&& port) -> decltype(auto)
    {
      return read(port);
    }

    auto read(let const& x) -> decltype(auto)
    {
      if (x.is<null>())
      {
        throw read_error<void>(__FILE__, ":", __LINE__);
      }
      else if (x.is<input_port>())
      {
        return read(x.as<input_port>());
      }
      else
      {
        throw read_error<void>(__FILE__, ":", __LINE__);
      }
    }

    auto read() -> decltype(auto)
    {
      return read(current_input_port());
    }

    auto read(const std::string& s) -> decltype(auto)
    {
      return read(open_input_string(s));
    }

  public:
    auto ready() // TODO RENAME TO 'char-ready'
    {
      return static_cast<bool>(
        current_input_port() and current_input_port().template as<input_port>());
    }

    let standard_input_port() const noexcept
    {
      let static port = make<input_port>("/dev/stdin");
      return port;
    }

    auto current_input_port() const noexcept -> decltype(auto)
    {
      // return sources.top();
      return standard_input_port();
    }

    // auto current_input_port(std::istream&& port)
    // {
    //   sources.push(port);
    //   return current_input_port();
    // }

    Define_Static_Perfect_Forwarding(open_input_file, std::ifstream);
    Define_Static_Perfect_Forwarding(open_input_string, std::stringstream);

  private:
    template <typename F>
    auto& ignore(std::istream& port, F&& predicate)
    {
      while (predicate(port.peek()))
      {
        port.ignore(1);
      }

      return port;
    }

    auto read_token(std::istream& port)
    {
      std::string token {};

      for (auto c { port.peek() }; not is_delimiter(c); c = port.peek())
      {
        token.push_back(port.get());
      }

      return token;
    }

    const object discriminate(std::istream& is)
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
        ignore(is, [](auto&& x) { return not is_delimiter(x); });
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
        ignore(is, [](auto&& x) { return not is_delimiter(x); });
        return t;

      case 'x':
        is.ignore(1);
        return make<exact_integer>("0x" + read_token(is));

      case '(':
        if (let const xs { read(is) }; xs.is<null>())
        {
          return make<vector>();
        }
        else
        {
          return make<vector>(in_range, xs);
        }

      case '\\':
        {
          is.ignore(1);

          auto name { read_token(is) };

          if (name.empty())
          {
            name.push_back(is.get());
          }

          static const std::unordered_map<std::string, char> names
          {
            { "alarm"    , 0x07 },
            { "backspace", 0x08 },
            { "delete"   , 0x7F },
            { "escape"   , 0x1B },
            { "newline"  , 0x0A },
            { "null"     , 0x00 },
            { "return"   , 0x0D },
            { "space"    , 0x20 },
            { "tab"      , 0x09 },
          };

          if (const auto iter { names.find(name) }; iter != std::end(names))
          {
            return make<character>(cdr(*iter));
          }
          else
          {
            return make<character>(name);
          }
        }

      default:
        is.ignore(1);
        return unspecified; // XXX
      }
    }
  };
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP
