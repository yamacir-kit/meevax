#ifndef INCLUDED_MEEVAX_KERNEL_READER_HPP
#define INCLUDED_MEEVAX_KERNEL_READER_HPP

#include <limits> // std::numeric_limits<std::streamsize>
#include <sstream>
#include <stack>
#include <type_traits>

// #include <boost/iostreams/device/null.hpp>
// #include <boost/iostreams/stream_buffer.hpp>

#include <boost/lexical_cast.hpp>
#include <meevax/iostream/ignore.hpp>
#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/miscellaneous.hpp>
#include <meevax/kernel/numeric_io.hpp>
#include <meevax/kernel/parser.hpp> // DEPRECATED
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
  auto read_token(input_port & port) -> std::string;

  // TODO Move into reader class private
  let read_char(input_port &);

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

    IMPORT(SK, evaluate,            NIL);
    IMPORT(SK, intern,              NIL);
    IMPORT(SK, standard_debug_port, NIL);
    IMPORT(SK, write_to,            NIL);

    using char_type = typename input_port::char_type;

    using seeker = std::istream_iterator<char_type>;

    template <input_port::char_type C>
    using reserved_character = std::integral_constant<decltype(C), C>;

    using period               = reserved_character<'.'>;
    using left_parenthesis     = reserved_character<'('>;
    using right_parenthesis    = reserved_character<')'>;
    using left_square_bracket  = reserved_character<'['>;
    using right_square_bracket = reserved_character<']'>;
    using left_curly_bracket   = reserved_character<'{'>;
    using right_curly_bracket  = reserved_character<'}'>;

  public:
    /* ---- Read ---------------------------------------------------------------
     *
     *
     * ---------------------------------------------------------------------- */
    let const read(input_port & port)
    {
      std::basic_string<char_type> token {};

      for (seeker head = port; head != seeker(); ++head)
      {
        switch (auto const c = *head)
        {
        case ';':
          port.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
          break;

        case ' ': case '\f': case '\n': case '\r': case '\t': case '\v':
          break;

        case '(':
        case '[':
        case '{':
          try
          {
            let const kar = read(port);
            port.putback(c);
            return cons(kar, read(port));
          }
          catch (read_error<right_parenthesis> const&)
          {
            return char_eq(c, '(') ? unit : throw;
          }
          catch (read_error<right_square_bracket> const&)
          {
            return char_eq(c, '[') ? unit : throw;
          }
          catch (read_error<right_curly_bracket> const&)
          {
            return char_eq(c, '{') ? unit : throw;
          }
          catch (read_error<period> const&)
          {
            let const kdr = read(port);

            switch (c)
            {
            case '(': ignore(port, [](auto each) { return not char_eq(each, ')'); }).get(); break;
            case '[': ignore(port, [](auto each) { return not char_eq(each, ']'); }).get(); break;
            case '{': ignore(port, [](auto each) { return not char_eq(each, '}'); }).get(); break;
            }

            return kdr;
          }

        case ')':
          throw read_error<right_parenthesis>("unexpected ", std::quoted(")"));

        case ']':
          throw read_error<right_square_bracket>("unexpected ", std::quoted("]"));

        case '}':
          throw read_error<right_curly_bracket>(c, " is reserved for possible future extensions to the language.");

        case '#':
          return discriminate(port);

        case '"':
          return make<string>(port);

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

          if (auto const c = port.peek(); is_end_of_token(c))
          {
            if (token == ".")
            {
              throw read_error<period>("dot-notation");
            }
            else try
            {
              return to_number(token, 10);
            }
            catch (...)
            {
              return intern(token);
            }
          }
        }
      }

      return eof_object;
    }

    decltype(auto) read(object const& x)
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

    decltype(auto) read(std::basic_string<char_type> const& s)
    {
      std::basic_stringstream<char_type> ss { s };
      return read(ss);
    }

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
      switch (auto const discriminator = is.get())
      {
      case '!':
        is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        return read(is);

      case ',': // from SRFI-10
        return evaluate(read(is));

      case ';': // from SRFI-62
        return read(is), read(is);

      case 'b': // (string->number (read) 2)
        return to_number(is.peek() == '#' ? boost::lexical_cast<std::string>(read(is)) : read_token(is), 2);

      case 'c': // from Common Lisp
        if (let const xs = read(is); not xs.is<pair>())
        {
          return make<complex>(make<exact_integer>(0), make<exact_integer>(0));
        }
        else if (not cdr(xs).is<pair>())
        {
          return make<complex>(car(xs), make<exact_integer>(0));
        }
        else
        {
          return make<complex>(car(xs), cadr(xs));
        }

      case 'd':
        return to_number(is.peek() == '#' ? boost::lexical_cast<std::string>(read(is)) : read_token(is), 10);

      case 'e':
        return exact(read(is)); // NOTE: Same as #,(exact (read))

      case 'f':
        ignore(is, [](auto&& x) { return not is_end_of_token(x); });
        return f;

      case 'i':
        return inexact(read(is)); // NOTE: Same as #,(inexact (read))

      case 'o':
        return to_number(is.peek() == '#' ? boost::lexical_cast<std::string>(read(is)) : read_token(is), 8);

      case 'p':
        assert(is.get() == '"');
        is.ignore(1);
        return make<path>(string(is));

      case 't':
        ignore(is, [](auto&& x) { return not is_end_of_token(x); });
        return t;

      case 'x':
        return to_number(is.peek() == '#' ? boost::lexical_cast<std::string>(read(is)) : read_token(is), 16);

      case '(':
        is.putback(discriminator);
        return make<vector>(for_each_in, read(is));

      case '\\':
        return read_char(is);

      default:
        throw read_error<void>("unknown <discriminator>: #", discriminator);
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP
