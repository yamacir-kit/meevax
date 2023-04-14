/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  auto get_codepoint(std::istream &) -> character::int_type;

  auto get_token(std::istream &) -> std::string;

  auto ignore_nested_block_comment(std::istream &) -> std::istream &;

  template <typename T>
  auto read(std::istream &) -> object;

  template <> auto read<character>(std::istream &) -> object;
  template <> auto read<string   >(std::istream &) -> object;

  auto string_to_integer (std::string const&, int = 10) -> object;
  auto string_to_rational(std::string const&, int = 10) -> object;
  auto string_to_real    (std::string const&, int = 10) -> object;
  auto string_to_complex (std::string const&, int = 10) -> object;
  auto string_to_number  (std::string const&, int = 10) -> object;

  template <typename Environment>
  class reader
  {
    friend Environment;

    explicit constexpr reader()
    {}

    struct datum_label
    {
      std::uintptr_t value;
    };

    std::unordered_map<std::uintptr_t, object> datum_labels;

    auto finish(object const& xs, object const& datum) -> void
    {
      if (xs.is<pair>())
      {
        finish(car(xs), datum);

        if (cdr(xs).is<datum_label>())
        {
          cdr(xs) = datum;
        }
        else
        {
          finish(cdr(xs), datum);
        }
      }
    }

  public:
    using char_type = typename std::istream::char_type;

    auto get_ready() const
    {
      return static_cast<bool>(std::cin);
    }

    auto read(std::istream & is = std::cin) -> object
    {
      for (auto head = std::istream_iterator<char_type>(is); head != std::istream_iterator<char_type>(); ++head)
      {
        switch (auto const c = *head)
        {
        case '\t': // 0x09
        case '\n': // 0x0A
        case '\v': // 0x0B
        case '\f': // 0x0C
        case '\r': // 0x0D
        case ' ':  // 0x20
          break;

        case '"':  // 0x22
          return meevax::read<string>(is.putback(c));

        case '#':  // 0x23
          switch (auto const c = is.get())
          {
          case '!': // SRFI 22
            is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            return read(is);

          case ',': // SRFI 10
            return static_cast<Environment &>(*this).evaluate(read(is));

          case ';': // SRFI 62
            return read(is), read(is);

          case '"':
            return string_to_symbol(meevax::read<string>(is.putback(c)).as<string>());

          case '0':
          case '1':
          case '2':
          case '3':
          case '4':
          case '5':
          case '6':
          case '7':
          case '8':
          case '9':
            if (std::uintptr_t n = 0; is.putback(c) >> n)
            {
              switch (auto c = is.get())
              {
              case '#':
                return datum_labels.at(n);

              case '=':
                if (auto && [iter, success] = datum_labels.emplace(n, make<datum_label>(n)); success)
                {
                  let result = read(is);

                  finish(result, result);

                  datum_labels.erase(n);

                  return result;
                }
                else
                {
                  throw read_error(make<string>("duplicated datum-label declaration"),
                                   make<exact_integer>(n));
                }

              default:
                throw read_error(make<string>("unknown discriminator"),
                                 make<string>(lexical_cast<std::string>("#", n, std::char_traits<char_type>::to_char_type(c))));
              }
            }
            else
            {
              return eof_object;
            }

          case 'b':
            return string_to_number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : get_token(is), 2);

          case 'c': // Common Lisp
            return [](let const& xs)
            {
              return make<complex>(tail(xs, 0).is<pair>() ? xs[0] : e0,
                                   tail(xs, 1).is<pair>() ? xs[1] : e0);
            }(read(is));

          case 'd':
            return string_to_number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : get_token(is), 10);

          case 'e':
            return apply_arithmetic<exact>(read(is)); // NOTE: Same as #,(exact (read))

          case 'f':
            get_token(is);
            return f;

          case 'i':
            return apply_arithmetic<inexact>(read(is)); // NOTE: Same as #,(inexact (read))

          case 'o':
            return string_to_number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : get_token(is), 8);

          case 't':
            get_token(is);
            return t;

          case 'x':
            return string_to_number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : get_token(is), 16);

          case '(':
            is.putback(c);
            return make<vector>(read(is));

          case '\\':
            return meevax::read<character>(is);

          case '|': // SRFI 30
            ignore_nested_block_comment(is);
            return read(is);

          default:
            throw read_error(make<string>("unknown discriminator"), make<character>(c));
          }

        case '\'': // 0x27
          return list(string_to_symbol("quote"), read(is));

        case '(':  // 0x28
          try
          {
            if (let const& x = read(is); x == eof_object)
            {
              return x;
            }
            else
            {
              return cons(x, read(is.putback(c)));
            }
          }
          catch (std::integral_constant<char_type, ')'> const&)
          {
            return unit;
          }
          catch (std::integral_constant<char_type, '.'> const&)
          {
            let const kdr = read(is);
            is.ignore(std::numeric_limits<std::streamsize>::max(), ')');
            return kdr;
          }

        case ')':  // 0x29
          throw std::integral_constant<char_type, ')'>();

        case ',':  // 0x2C
          switch (is.peek())
          {
          case '@':
            is.ignore(1);
            return list(string_to_symbol("unquote-splicing"), read(is));

          default:
            return list(string_to_symbol("unquote"), read(is));
          }

        case ';':  // 0x3B
          is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
          break;

        case '`':  // 0x60
          return list(string_to_symbol("quasiquote"), read(is));

        case '|':  // 0x7C
          return string_to_symbol(meevax::read<string>(is.putback(c)).as<string>());

        case '[':  // 0x5B
        case ']':  // 0x5D
        case '{':  // 0x7B
        case '}':  // 0x7D
          throw read_error(make<string>("left and right square and curly brackets (braces) are reserved for possible future extensions to the language"),
                           make<string>("\\#" + c));

        default:
          if (auto const& token = get_token(is.putback(c)); token == ".")
          {
            throw std::integral_constant<char_type, '.'>();
          }
          else try
          {
            return string_to_number(token, 10);
          }
          catch (...)
          {
            return string_to_symbol(token);
          }
        }
      }

      return eof_object;
    }

    auto read(std::string const& s) -> decltype(auto)
    {
      auto port = std::stringstream(s);
      return read(port);
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP
