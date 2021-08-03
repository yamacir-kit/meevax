/*
   Copyright 2018-2021 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef INCLUDED_MEEVAX_KERNEL_READER_HPP
#define INCLUDED_MEEVAX_KERNEL_READER_HPP

// #include <boost/iostreams/device/null.hpp>
// #include <boost/iostreams/stream_buffer.hpp>

#include <boost/lexical_cast.hpp>
#include <meevax/iostream/ignore.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/numeric_io.hpp>
#include <meevax/kernel/parser.hpp> // DEPRECATED
#include <meevax/kernel/port.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  auto read_token(std::istream &) -> std::string;

  // TODO Move into reader class private
  auto read_char(std::istream &) -> let;

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

    using char_type = typename std::istream::char_type;

    using seeker = std::istream_iterator<char_type>;

    template <char_type C>
    using char_constant = std::integral_constant<char_type, C>;

  public:
    /* ---- Read ---------------------------------------------------------------
     *
     *
     * ---------------------------------------------------------------------- */
    auto read(std::istream & is) -> let
    {
      std::string token {};

      for (seeker head = is; head != seeker(); ++head)
      {
        switch (auto const c = *head)
        {
        case ';':
          is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
          break;

        case ' ': case '\f': case '\n': case '\r': case '\t': case '\v':
          break;

        case '(':
        case '[':
        case '{':
          try
          {
            let const kar = read(is);
            is.putback(c);
            return cons(kar, read(is));
          }
          catch (tagged_read_error<char_constant<')'>> const&) { return char_eq(c, '(') ? unit : throw; }
          catch (tagged_read_error<char_constant<']'>> const&) { return char_eq(c, '[') ? unit : throw; }
          catch (tagged_read_error<char_constant<'}'>> const&) { return char_eq(c, '{') ? unit : throw; }
          catch (tagged_read_error<char_constant<'.'>> const&)
          {
            let const kdr = read(is);

            switch (c)
            {
            case '(': ignore(is, [](auto c) { return not char_eq(c, ')'); }).get(); break;
            case '[': ignore(is, [](auto c) { return not char_eq(c, ']'); }).get(); break;
            case '{': ignore(is, [](auto c) { return not char_eq(c, '}'); }).get(); break;
            }

            return kdr;
          }

        case ')':
          throw tagged_read_error<char_constant<')'>>(make<string>("unexpected character: "), make<character>(c));

        case ']':
          throw tagged_read_error<char_constant<']'>>(make<string>("unexpected character: "), make<character>(c));

        case '}':
          throw tagged_read_error<char_constant<'}'>>(make<string>("unexpected character: "), make<character>(c));

        case '#':
          return discriminate(is);

        case '"':
          return make<string>(is);

        case '\'':
          return list(intern("quote"), read(is));

        case '`':
          return list(intern("quasiquote"), read(is));

        case ',':
          switch (is.peek())
          {
          case '@':
            is.ignore(1);
            return list(intern("unquote-splicing"), read(is));

          default:
            return list(intern("unquote"), read(is));
          }

        default:
          if (token.push_back(c); is_end_of_token(is.peek()))
          {
            if (token == ".")
            {
              throw tagged_read_error<char_constant<'.'>>(make<string>("unexpected character: "), make<character>('.'));
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

    auto read(let const& x) -> let
    {
      if (x.is_polymorphically<std::istream>())
      {
        return read(x.as<std::istream>());
      }
      else
      {
        throw read_error(make<string>("not an input-port: "), x);
      }
    }

    auto read() -> let
    {
      let const result = read(default_input_port);

      write_to(standard_debug_port(), "\n", header(__func__), result, "\n");

      return result;
    }

    auto read(std::string const& s) -> decltype(auto)
    {
      std::stringstream ss { s };

      return read(ss);
    }

    auto ready() // TODO RENAME TO 'char-ready'
    {
      return default_input_port.is_polymorphically<std::istream>() and default_input_port.as<std::istream>();
    }

  private:
    let const discriminate(std::istream & is) // TODO MOVE INTO read
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
        throw read_error(make<string>("unknown <discriminator>: "), make<character>(discriminator));
      }
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP
