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
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/homogeneous_vector.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/standard_input_port.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  struct datum_label
  {
    std::string const n;

    template <typename... Ts>
    explicit datum_label(Ts&&... xs)
      : n { std::forward<decltype(xs)>(xs)... }
    {}
  };

  auto circulate(object const&, std::string const&) -> void;

  auto make_integer (std::string const&, int = 10) -> object;
  auto make_rational(std::string const&, int = 10) -> object;
  auto make_real    (std::string const&, int = 10) -> object;
  auto make_complex (std::string const&, int = 10) -> object;
  auto make_number  (std::string const&, int = 10) -> object;

  template <typename Environment>
  struct reader
  {
    using char_type = typename std::istream::char_type;

    std::unordered_map<std::string, object> datum_labels;

    auto read(textual_input_port & input) -> object
    {
      auto & is = static_cast<std::istream &>(input);

      auto take_surrounded = [&]()
      {
        auto s = string();

        auto const quotation_mark = is.get();

        for (auto codepoint = input.take_codepoint(); not character::is_eof(codepoint); codepoint = input.take_codepoint())
        {
          if (codepoint == quotation_mark)
          {
            return s;
          }
          else switch (codepoint)
          {
          case '\\':
            switch (auto const codepoint = input.take_codepoint(); codepoint)
            {
            case 'a': s.codepoints.emplace_back('\a'); break;
            case 'b': s.codepoints.emplace_back('\b'); break;
            case 'f': s.codepoints.emplace_back('\f'); break;
            case 'n': s.codepoints.emplace_back('\n'); break;
            case 'r': s.codepoints.emplace_back('\r'); break;
            case 't': s.codepoints.emplace_back('\t'); break;
            case 'v': s.codepoints.emplace_back('\v'); break;
            case 'x':
              if (auto token = std::string(); std::getline(is, token, ';'))
              {
                s.codepoints.emplace_back(lexical_cast<character::int_type>(std::hex, token));
              }
              break;

            case '\n':
            case '\r':
              while (std::isspace(is.peek()))
              {
                is.ignore(1);
              }
              break;

            default:
              s.codepoints.emplace_back(codepoint);
              break;
            }
            break;

          default:
            s.codepoints.emplace_back(codepoint);
            break;
          }
        }

        throw read_error(make<string>("An end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable"));
      };

      auto read_character = [&]()
      {
        std::unordered_map<std::string, character::int_type> static const names {
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

        switch (auto token = input.take_token(); token.length())
        {
        case 0:
          // assert(is_special_character(is.peek()));
          return make<character>(is.get());

        case 1:
          assert(std::isprint(token.front()));
          return make<character>(token.front());

        default:
          if (auto iter = names.find(token); iter != std::end(names))
          {
            return make<character>(iter->second);
          }
          else if (token[0] == 'x' and 1 < token.length())
          {
            return make<character>(lexical_cast<character::int_type>(std::hex, token.substr(1)));
          }
          else
          {
            throw read_error(make<string>("not a character"), make<string>("\\#" + token));
          }
        }
      };

      while (not character::is_eof(is.peek()))
      {
        switch (auto const c1 = is.peek())
        {
        case '\t': // 0x09
        case '\n': // 0x0A
        case '\v': // 0x0B
        case '\f': // 0x0C
        case '\r': // 0x0D
        case ' ':  // 0x20
          is.ignore(1);
          break;

        case '"':  // 0x22
          return make(take_surrounded());

        case '#':  // 0x23
          switch (auto const c2 = is.ignore(1).peek())
          {
          case '!': // SRFI 22
            is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            return read(input);

          case ',': // SRFI 10
            is.ignore(1);
            return static_cast<Environment &>(*this).evaluate(read(input));

          case ';': // SRFI 62
            is.ignore(1);
            read(input); // Discard an expression.
            return read(input);

          case '"':
            return make_symbol(take_surrounded());

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
            switch (auto n = input.take_digits(); is.peek())
            {
            case '#':
              is.ignore(1);

              if (auto iter = datum_labels.find(n); iter != std::end(datum_labels))
              {
                return iter->second;
              }
              else
              {
                throw read_error(make<string>("it is an error to attempt a forward reference"),
                                 make<string>(lexical_cast<std::string>('#', n, '#')));
              }

            case '=':
              is.ignore(1);

              if (auto [iter, success] = datum_labels.emplace(n, make<datum_label>(n)); success)
              {
                if (let const& xs = read(input); xs != iter->second)
                {
                  circulate(xs, n);
                  datum_labels.erase(n);
                  return xs;
                }
                else
                {
                  return unit;
                }
              }
              else
              {
                throw read_error(make<string>("duplicated datum-label declaration"),
                                 make<string>(n));
              }

            default:
              throw read_error(make<string>("unknown discriminator"),
                               make<string>(lexical_cast<std::string>('#', n, is.get())));
            }

          case 'b':
            return make_number(is.ignore(1).peek() == '#' ? lexical_cast<std::string>(read(input)) : input.take_token(), 2);

          case 'c': // Common Lisp
            is.ignore(1);
            return [](let const& xs)
            {
              return make<complex>(tail(xs, 0).is<pair>() ? xs[0] : e0,
                                   tail(xs, 1).is<pair>() ? xs[1] : e0);
            }(read(input));

          case 'd':
            return make_number(is.ignore(1).peek() == '#' ? lexical_cast<std::string>(read(input)) : input.take_token(), 10);

          case 'e':
            is.ignore(1);
            return exact(read(input)); // NOTE: Same as #,(exact (read))

          case 'f':
            is.ignore(1);

            switch (auto const digits = input.take_digits(); std::stoi(digits))
            {
            case 32:
              return make<f32vector>(read(input));

            case 64:
              return make<f64vector>(read(input));

            default:
              input.take_token();
              return f;
            }

          case 'i':
            is.ignore(1);
            return inexact(read(input)); // NOTE: Same as #,(inexact (read))

          case 'o':
            return make_number(is.ignore(1).peek() == '#' ? lexical_cast<std::string>(read(input)) : input.take_token(), 8);

          case 's':
            is.ignore(1);

            switch (auto const digits = input.take_digits(); std::stoi(digits))
            {
            case 8:
              return make<s8vector>(read(input));

            case 16:
              return make<s16vector>(read(input));

            case 32:
              return make<s32vector>(read(input));

            case 64:
              return make<s64vector>(read(input));

            default:
              throw read_error(make<string>("An unknown literal expression was encountered"),
                               make<string>(lexical_cast<std::string>("#s", digits)));
            }

          case 't':
            input.take_token();
            return t;

          case 'u':
            is.ignore(1);

            switch (auto const digits = input.take_digits(); std::stoi(digits))
            {
            case 8:
              return make<u8vector>(read(input));

            case 16:
              return make<u16vector>(read(input));

            case 32:
              return make<u32vector>(read(input));

            case 64:
              return make<u64vector>(read(input));

            default:
              throw read_error(make<string>("An unknown literal expression was encountered"),
                               make<string>(lexical_cast<std::string>("#u", digits)));
            }

          case 'x':
            return make_number(is.ignore(1).peek() == '#' ? lexical_cast<std::string>(read(input)) : input.take_token(), 16);

          case '(':
            return make<vector>(read(input));

          case '\\':
            is.ignore(1);
            return read_character();

          case '|': // SRFI 30
            is.ignore(1);
            input.take_nested_block_comment();
            return read(input);

          default:
            throw read_error(make<string>("unknown discriminator"), make<character>(c2));
          }

        case '\'': // 0x27
          is.ignore(1);
          return list(make_symbol("quote"), read(input));

        case '(':  // 0x28
          try
          {
            is.ignore(1);

            if (let const& x = read(input); x.is<eof>())
            {
              return x;
            }
            else
            {
              is.putback('('); // modifying putback (https://en.cppreference.com/w/cpp/io/basic_istream/putback)
              return cons(x, read(input));
            }
          }
          catch (std::integral_constant<char_type, ')'> const&)
          {
            return unit;
          }
          catch (std::integral_constant<char_type, '.'> const&)
          {
            let const x = read(input);
            is.ignore(std::numeric_limits<std::streamsize>::max(), ')');
            return x;
          }

        case ')':  // 0x29
          is.ignore(1);
          throw std::integral_constant<char_type, ')'>();

        case ',':  // 0x2C
          switch (is.ignore(1); is.peek())
          {
          case '@':
            is.ignore(1);
            return list(make_symbol("unquote-splicing"), read(input));

          default:
            return list(make_symbol("unquote"), read(input));
          }

        case ';':  // 0x3B
          is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
          break;

        case '`':  // 0x60
          is.ignore(1);
          return list(make_symbol("quasiquote"), read(input));

        case '|':  // 0x7C
          return make_symbol(take_surrounded());

        case '[':  // 0x5B
        case ']':  // 0x5D
        case '{':  // 0x7B
        case '}':  // 0x7D
          throw read_error(make<string>("left and right square and curly brackets (braces) are reserved for possible future extensions to the language"),
                           make<character>(c1));

        default:
          if (auto const& token = input.take_token(); token == ".")
          {
            throw std::integral_constant<char_type, '.'>();
          }
          else try
          {
            return make_number(token, 10);
          }
          catch (std::invalid_argument const&)
          {
            return make_symbol(token);
          }
        }
      }

      return eof_object;
    }

    auto read() -> decltype(auto)
    {
      auto input = standard_input_port();
      return read(input);
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP
