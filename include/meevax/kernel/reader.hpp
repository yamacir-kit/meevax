#ifndef INCLUDED_MEEVAX_KERNEL_READER_HPP
#define INCLUDED_MEEVAX_KERNEL_READER_HPP

#include <istream>
#include <limits> // std::numeric_limits<std::streamsize>
#include <sstream>
#include <stack>

#include <boost/iostreams/device/null.hpp>
#include <boost/iostreams/stream_buffer.hpp>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax { inline namespace kernel
{
  /* ==== EOF ==================================================================
  *
  *
  * ========================================================================= */
  struct eof
  {
    friend auto operator<<(std::ostream& os, const eof&)
      -> decltype(auto)
    {
      return os << console::magenta << "#,("
                << console::green   << "eof-object"
                << console::magenta << ")"
                << console::reset;
    }
  };

  static const auto eof_object {make<eof>()};

  /* ==== EOS ==================================================================
  *
  *
  * ========================================================================= */
  struct eos
  {
    friend auto operator<<(std::ostream& os, const eos&)
      -> decltype(auto)
    {
      return os << console::magenta << "#,("
                << console::green   << "eos-object"
                << console::magenta << ")"
                << console::reset;
    }
  };

  static const auto eos_object {make<eos>()};

  /* ==== String Constructor ===================================================
  *
  *
  * ========================================================================= */
  auto read_string(std::istream& port) -> const object
  {
    switch (auto c {port.narrow(port.get(), '\0')}; c)
    {
    case '"': // Right Double Quotation
      return unit;

    case '\\': // Escape Sequences
      switch (auto c {port.narrow(port.get(), '\0')}; c)
      {
      case 'a':
        return make<string>(characters.at("bell"), read_string(port));

      case 'b':
        return make<string>(characters.at("backspace"), read_string(port));

      case 'n':
        return make<string>(characters.at("line-feed"), read_string(port));

      case 'r':
        return make<string>(characters.at("carriage-return"), read_string(port));

      case 't':
        return make<string>(characters.at("horizontal-tabulation"), read_string(port));

      case '|':
        return make<string>(make<character>("|"), read_string(port));

      case '"':
        return make<string>(make<character>("\""), read_string(port));

      case '\\':
        return make<string>(make<character>("\\"), read_string(port));

      case '\r':
      case '\n':
        while (is_intraline_whitespace(port.peek()))
        {
          port.ignore(1);
        }
        return read_string(port);

      default:
        return make<string>(make<character>(std::string(1, '\0')), read_string(port));
      }

    default:
      return make<string>(make<character>(std::string(1, c)), read_string(port));
    }
  }

  auto make_string(const std::string& s) -> decltype(auto)
  {
    std::stringstream port {};
    port << s << "\"";
    return read_string(port);
  }

  /* ==== Number Constructor ===================================================
   *
   *
   * ======================================================================== */
  namespace regex
  {
    template <std::size_t R>
    auto digit() -> const std::string;

    template <> auto digit< 2>() -> const std::string { return "[01]"; };
    template <> auto digit< 8>() -> const std::string { return "[01234567]"; };
    template <> auto digit<10>() -> const std::string { return "\\d"; };
    template <> auto digit<16>() -> const std::string { return "[" + digit<10>() + "abcdef]"; };

    template <std::size_t R>
    auto digits(const std::string& quantifier)
    {
      return digit<R>() + quantifier;
    }

    template <std::size_t R>
    auto radix() -> const std::string;

    template <> auto radix< 2>() -> const std::string { return  "#b";   }
    template <> auto radix< 8>() -> const std::string { return  "#o";   }
    template <> auto radix<10>() -> const std::string { return "(#d)?"; }
    template <> auto radix<16>() -> const std::string { return  "#x";   }

    auto exactness() -> const std::string
    {
      return "(#e|#i)?";
    };

    auto sign() -> const std::string
    {
      return "[\\+-]?";
    };

    auto infnan() -> const std::string
    {
      return "[\\+-](inf|nan)\\.0";
    };

    auto suffix() -> const std::string
    {
      return "(e" + sign() + digits<10>("+") + ")?";
    };

    template <std::size_t R = 10>
    auto prefix() -> const std::string
    {
      return "(" + radix<R>() + exactness() + "|" + exactness() + radix<R>() + ")";
    };

    template <std::size_t R = 10>
    auto unsigned_integer() -> const std::string
    {
      return digits<R>("+");
    };

    template <std::size_t R = 10>
    auto signed_integer() -> const std::string
    {
      return sign() + unsigned_integer<R>();
    };

    template <std::size_t R = 10>
    auto decimal() -> const std::string
    {
      return "(" + unsigned_integer<R>()                   + suffix() +
             "|"                    "\\." + digits<R>("+") + suffix() +
             "|" + digits<R>("+") + "\\." + digits<R>("*") + suffix() + ")";
    };

    template <std::size_t R>
    auto unsigned_real() -> const std::string
    {
      return "(" + unsigned_integer<R>()                               +
             "|" + unsigned_integer<R>() + "/" + unsigned_integer<R>() +
             "|" +          decimal<R>()                               + ")";
    };

    template <std::size_t R = 10>
    auto signed_real() -> const std::string
    {
      return "(" + sign() + unsigned_real<R>() + "|" + infnan() + ")";
    };

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
    };

    template <std::size_t R = 10>
    auto number() -> const std::string
    {
      return prefix<R>() + signed_complex<R>();
    };
  } // inline namespace regex

  template <std::size_t R = 10>
  auto is_number(const std::string& token)
  {
    static const std::regex pattern { regex::number<R>() };
    std::smatch result {};
    return std::regex_match(token, result, pattern);
  }

  template <std::size_t R = 10>
  auto make_number(const std::string& token) -> const object
  {
    static const std::unordered_map<std::string, object> srfi_144
    {
      std::make_pair("fl-pi", make<decimal<most_precise>>(boost::math::constants::pi<decimal<most_precise>::value_type>()))
    };

    static const std::regex pattern { regex::number<R>() };

    if (const auto iter { srfi_144.find(token) }; iter != std::end(srfi_144))
    {
      return cdr(*iter);
    }
    else if (std::smatch result {}; std::regex_match(token, result, pattern))
    {
      if (result.length(30)) // 6, 30, 31, 32, 38, 39
      {
        return make<complex>(make_number<R>(result.str(31)), make_number<R>(result.str(38)));
      }

      if (result.length(53)) // 6, 53, 54, 55, 61, 62
      {
        return make<complex>(make_number<R>(result.str(54)), make_number<R>(result.str(61)));
      }

      if (result.length(14)) // 6, 7, 8, 14
      {
        static const std::unordered_map<std::string, object> infnan
        {
          std::make_pair("+inf.0", make<decimal<most_precise>>(+decimal<most_precise>::infinity())),
          std::make_pair("-inf.0", make<decimal<most_precise>>(-decimal<most_precise>::infinity())),
          std::make_pair("+nan.0", make<decimal<most_precise>>(+decimal<most_precise>::quiet_NaN())),
          std::make_pair("-nan.0", make<decimal<most_precise>>(-decimal<most_precise>::quiet_NaN()))
        };

        return infnan.at(token);
      }

      if (result.length(10)) // 6, 7, 8, 9, 10
      {
        return make<decimal<64>>(token.substr(token[0] == '+' ? 1 : 0));
      }

      if (result.length(9)) // 6, 7, 8, 9
      {
        return make<integer>(token.substr(token[0] == '+' ? 1 : 0));
      }

      for (auto iter { std::begin(result) }; iter != std::end(result); ++iter)
      {
        if ((*iter).length())
        {
          switch (auto index { std::distance(std::begin(result), iter) }; index)
          {
          default:
            std::cout << "; number[" << index << "/" << result.size() << "] = " << *iter << std::endl;
            break;
          }
        }
      }

      std::stringstream port {};
      port << "the given token '" << token << "' is a valid Scheme numeric literal, but Meevax is not yet supported.";
      throw std::runtime_error { port.str() };
    }
    else
    {
      std::stringstream port {};
      port << "the given token '" << token << "' is a invalid Scheme numeric literal.";
      throw std::runtime_error { port.str() };
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

    using seeker
      = std::istream_iterator<std::istream::char_type>;

  protected:
    // NOTE
    // std::stack<object> sources {};
    // auto path {read_string("/path/to/file")};
    // auto something {make<input_port>(path.as<std::string>())};
    // car(something) = path;
    // cdr(something) = make<integer>(1); // current line

    // std::stack<std::istream> sources;

  public:
    /* ==== Read ===============================================================
    *
    * TODO
    *   Rename read(std::istream&) => read_from
    *
    * ======================================================================= */
    auto read(std::istream& port) -> const object
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
        catch (const reader_error_about_parentheses&)
        {
          return unit;
        }
        catch (const reader_error_about_pair&)
        {
          auto expression {read(port)};
          port.ignore(std::numeric_limits<std::streamsize>::max(), ')'); // XXX DIRTY HACK
          return expression;
        }

      case ')':
        throw reader_error_about_parentheses { "unexpected close parentheses inserted" };

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
            throw reader_error_about_pair {"dot-notation"};
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

    auto read() -> decltype(auto)
    {
      return read(current_input_port());
    }

    auto read(const std::string& s) -> decltype(auto)
    {
      return read(open_input_string(s));
    }

  public:
    auto ready()
    {
      return static_cast<bool>(current_input_port());
    }

    auto standard_input_port() const noexcept -> auto&
    {
      return std::cin;
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

        if (const auto xs { read(is) }; null(xs) or not xs.template is<pair>())
        {
          return make<complex>(make<integer>(0), make<integer>(0));
        }
        else if (null(cdr(xs)) or not cdr(xs).template is<pair>())
        {
          return make<complex>(car(xs), make<integer>(0));
        }
        else
        {
          return make<complex>(car(xs), cadr(xs));
        }

      case 'd':
        is.ignore(1);
        return make<integer>(read_token(is));

      case 'f':
        ignore(is, [](auto&& x) { return not is_delimiter(x); });
        return f;

      case 'o':
        is.ignore(1);
        return make<integer>("0" + read_token(is));

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
        return make<integer>("0x" + read_token(is));

      case '(':
        if (const auto xs { read(is) }; null(xs))
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

          // NOTE Provide user-defined character-name?
          static const std::unordered_map<std::string, std::string> alias
          {
            {" ", "space"}, // for R7RS
            {"alarm", "bell"}, // for R7RS
            {"newline", "line-feed"}, // for R7RS
            {"return", "carriage-return"}, // for R7RS
            {"tab", "horizontal-tabulation"}, // for R7RS
          };

          // NOTE DIRTY HACK!
          if (auto iter {alias.find(name)}; iter != std::end(alias))
          {
            name = cdr(*iter);
          }

          // TODO Provide datum<character>(name)?
          if (auto iter {characters.find(name)}; iter != std::end(characters))
          {
            return cdr(*iter);
          }
          else
          {
            throw reader_error_about_character {name, " is unknown character-name"};
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
