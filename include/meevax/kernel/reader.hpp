#ifndef INCLUDED_MEEVAX_KERNEL_READER_HPP
#define INCLUDED_MEEVAX_KERNEL_READER_HPP

#include <istream>
#include <sstream>

#include <limits> // std::numeric_limits<std::streamsize>
#include <stack>

#include <boost/iostreams/device/null.hpp>
#include <boost/iostreams/stream_buffer.hpp>

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/character.hpp>
#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/numerical.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/utility/perfect_forward.hpp>
#include <stdexcept>
#include <string>

namespace meevax::kernel
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
  auto read_string(std::istream& port)
    -> const object
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
        return
          make<string>(
            make<character>(std::string(1, '\0')),
            read_string(port));
      }

    default:
      return
        make<string>(
          make<character>(std::string(1, c)),
          read_string(port));
    }
  }

  auto read_string(const std::string& s)
    -> decltype(auto)
  {
    std::stringstream port {};
    port << s << "\"";
    return read_string(port);
  }

  /* ==== Reader ===============================================================
  *
  *
  * ========================================================================= */
  template <typename SK>
  class reader
    : private boost::iostreams::stream_buffer<boost::iostreams::null_sink>
  {
    friend SK;

    explicit reader()
      : sources {}
    {
      sources.emplace(
        std::cin.rdbuf());
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

    std::stack<std::istream> sources;

  public:
    /* ==== Read ===============================================================
    *
    * TODO
    *   Rename read(std::istream&) => read_from
    *
    * ======================================================================= */
    auto read(std::istream& port)
      -> const object
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
        throw reader_error_about_parentheses {
          "unexpected close parentheses inserted"
        };

      case '#':
        return discriminate(port);

      case '"':
        return read_string(port);

      case '\'':
        return
          list(
            intern("quote"),
            read(port));

      case '`':
        return
          list(
            intern("quasiquote"),
            read(port));

      case ',':
        switch (port.peek())
        {
        case '@':
          port.ignore(1);
          return
            list(
              intern("unquote-splicing"),
              read(port));

        default:
          return
            list(
              intern("unquote"),
              read(port));
        }

      default:
        token.push_back(*head);

        if (auto c {port.peek()}; is_delimiter(c)) // delimiter
        {
          if (token == ".")
          {
            throw reader_error_about_pair {"dot-notation"};
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

      return eof_object;
    }

    auto read(std::istream&& port)
      -> decltype(auto)
    {
      return read(port);
    }

    auto read()
      -> decltype(auto)
    {
      return
        read(
          current_input_port());
    }

    auto read(const std::string& s)
      -> decltype(auto)
    {
      return
        read(
          open_input_string(s));
    }

  public:
    auto ready()
    {
      return
        static_cast<bool>(
          current_input_port());
    }

    auto standard_input_port()
      -> decltype(auto)
    {
      return std::cin;
    }

    auto current_input_port()
      -> decltype(auto)
    {
      return sources.top();
    }

    auto current_input_port(std::istream&& port)
    {
      sources.push(port);
      return current_input_port();
    }

  public:
    Define_Static_Perfect_Forwarding(open_input_file, std::ifstream);
    Define_Static_Perfect_Forwarding(open_input_string, std::stringstream);

  private:
    const object discriminate(std::istream& is)
    {
      switch (is.peek())
      {
      case 'f':
        while (not is_delimiter(is.peek()))
        {
          is.ignore(1);
        }
        return f;

      case 't':
        while (not is_delimiter(is.peek()))
        {
          is.ignore(1);
        }
        return t;

      /* ==== Read-Time Evaluation =============================================
      *
      * From SRFI-10
      *
      * ===================================================================== */
      case ',':
        is.ignore(1);
        return evaluate(read(is));

      case '\\':
        {
          is.ignore(1);

          std::string name {};

          for (auto c {is.peek()}; not is_delimiter(c); c = is.peek())
          {
            name.push_back(is.get());
          }

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

      case ';':
        is.ignore(1);
        return read(is), read(is);

      default:
        is.ignore(1);
        return unspecified; // XXX
      }
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP

