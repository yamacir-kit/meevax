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

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/homogeneous_vector.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/textual_input_port.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  constexpr auto is_special_character(character::int_type c)
  {
    auto one_of = [c](auto... xs) constexpr
    {
      return (character::eq(c, xs) or ...);
    };

    return character::is_eof(c) or one_of('\t', // 0x09
                                          '\n', // 0x0A
                                          '\v', // 0x0B
                                          '\f', // 0x0C
                                          '\r', // 0x0D
                                          ' ',  // 0x20
                                          '"',  // 0x22
                                          '#',  // 0x23
                                          '\'', // 0x27
                                          '(',  // 0x28
                                          ')',  // 0x29
                                          ',',  // 0x2C
                                          ';',  // 0x3B
                                          '[',  // 0x5B
                                          ']',  // 0x5D
                                          '`',  // 0x60
                                          '{',  // 0x7B
                                          '|',  // 0x7C
                                          '}'); // 0x7D
  }

  struct datum_label
  {
    std::string const n;

    template <typename... Ts>
    explicit datum_label(Ts&&... xs)
      : n { std::forward<decltype(xs)>(xs)... }
    {}
  };

  auto circulate(object const& xs, object const& x, std::string const& n) -> void
  {
    if (xs.is<pair>())
    {
      circulate(car(xs), x, n);

      if (cdr(xs).is<datum_label>() and cdr(xs).as<datum_label>().n == n)
      {
        cdr(xs) = x;
      }
      else
      {
        circulate(cdr(xs), x, n);
      }
    }
  }

  auto circulate(object const& xs, std::string const& n) -> void
  {
    return circulate(xs, xs, n);
  }

  auto textual_input_port::get() -> object
  {
    if (auto c = take_codepoint(); character::is_eof(c))
    {
      return eof_object;
    }
    else
    {
      return make<character>(c);
    }
  }

  auto textual_input_port::get(std::size_t size) -> object
  {
    if (character::is_eof(static_cast<std::istream &>(*this).peek()))
    {
      return eof_object;
    }
    else
    {
      auto s = string();

      for (std::size_t i = 0; i < size and not character::is_eof(static_cast<std::istream &>(*this).peek()); ++i)
      {
        s.codepoints.emplace_back(take_codepoint());
      }

      return make(s);
    }
  }

  auto textual_input_port::get_line() -> object
  {
    if (auto s = std::string(); std::getline(static_cast<std::istream &>(*this), s).eof())
    {
      return eof_object;
    }
    else
    {
      return make<string>(s);
    }
  }

  auto textual_input_port::get_ready() const -> bool
  {
    return static_cast<bool>(static_cast<std::istream const&>(*this));
  }

  auto textual_input_port::peek() -> object
  {
    if (auto c = peek_codepoint(); character::is_eof(c))
    {
      return eof_object;
    }
    else
    {
      return make<character>(c);
    }
  }

  auto textual_input_port::peek_codepoint() -> character::int_type
  {
    auto g = static_cast<std::istream &>(*this).tellg();
    auto c = take_codepoint();
    static_cast<std::istream &>(*this).seekg(g);
    return c;
  }

  auto textual_input_port::read() -> object
  {
    auto & is = static_cast<std::istream &>(*this);

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
        return make(read_string_literal());

      case '#':  // 0x23
        switch (auto const c2 = is.ignore(1).peek())
        {
        case '!': // SRFI 22
          if (auto token = take_token(); token == "!fold-case")
          {
            fold_case = true;
          }
          else if (token == "!no-fold-case")
          {
            fold_case = false;
          }
          else
          {
            is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
          }

          return read();

        case ',': // SRFI 10
          is.ignore(1);
          return interaction_environment().as<environment>().evaluate(read());

        case ';': // SRFI 62
          is.ignore(1);
          read(); // Discard an expression.
          return read();

        case '"':
          return make_symbol(read_string_literal());

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
          switch (auto n = take_digits(); is.peek())
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
              if (let const& xs = read(); xs != iter->second)
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
          return make_number(is.ignore(1).peek() == '#' ? lexical_cast<std::string>(read()) : take_token(), 2);

        case 'd':
          return make_number(is.ignore(1).peek() == '#' ? lexical_cast<std::string>(read()) : take_token(), 10);

        case 'e':
          is.ignore(1);
          return exact(read()); // NOTE: Same as #,(exact (read))

        case 'f':
          is.ignore(1);

          switch (auto const digits = take_digits(); std::stoi(digits))
          {
          case 32:
            return make<f32vector>(read());

          case 64:
            return make<f64vector>(read());

          default:
            take_token();
            return f;
          }

        case 'i':
          is.ignore(1);
          return inexact(read()); // NOTE: Same as #,(inexact (read))

        case 'o':
          return make_number(is.ignore(1).peek() == '#' ? lexical_cast<std::string>(read()) : take_token(), 8);

        case 's':
          is.ignore(1);

          switch (auto const digits = take_digits(); std::stoi(digits))
          {
          case 8:
            return make<s8vector>(read());

          case 16:
            return make<s16vector>(read());

          case 32:
            return make<s32vector>(read());

          case 64:
            return make<s64vector>(read());

          default:
            throw read_error(make<string>("An unknown literal expression was encountered"),
                             make<string>(lexical_cast<std::string>("#s", digits)));
          }

        case 't':
          take_token();
          return t;

        case 'u':
          is.ignore(1);

          switch (auto const digits = take_digits(); std::stoi(digits))
          {
          case 8:
            return make<u8vector>(read());

          case 16:
            return make<u16vector>(read());

          case 32:
            return make<u32vector>(read());

          case 64:
            return make<u64vector>(read());

          default:
            throw read_error(make<string>("An unknown literal expression was encountered"),
                             make<string>(lexical_cast<std::string>("#u", digits)));
          }

        case 'x':
          return make_number(is.ignore(1).peek() == '#' ? lexical_cast<std::string>(read()) : take_token(), 16);

        case '(':
          return make<vector>(read());

        case '\\':
          is.putback(c1);
          return make(read_character_literal());

        case '|': // SRFI 30
          is.ignore(1);
          take_nested_block_comment();
          return read();

        default:
          throw read_error(make<string>("unknown discriminator"), make<character>(c2));
        }

      case '\'': // 0x27
        is.ignore(1);
        return list(make_symbol("quote"), read());

      case '(':  // 0x28
        try
        {
          is.ignore(1);

          if (let const& x = read(); x.is<eof>())
          {
            return x;
          }
          else
          {
            is.putback('('); // modifying putback (https://en.cppreference.com/w/cpp/io/basic_istream/putback)
            return cons(x, read());
          }
        }
        catch (std::integral_constant<char, ')'> const&)
        {
          return unit;
        }
        catch (std::integral_constant<char, '.'> const&)
        {
          let const x = read();
          is.ignore(std::numeric_limits<std::streamsize>::max(), ')');
          return x;
        }

      case ')':  // 0x29
        is.ignore(1);
        throw std::integral_constant<char, ')'>();

      case ',':  // 0x2C
        switch (is.ignore(1); is.peek())
        {
        case '@':
          is.ignore(1);
          return list(make_symbol("unquote-splicing"), read());

        default:
          return list(make_symbol("unquote"), read());
        }

      case ';':  // 0x3B
        is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        break;

      case '`':  // 0x60
        is.ignore(1);
        return list(make_symbol("quasiquote"), read());

      case '|':  // 0x7C
        return make_symbol(read_string_literal());

      case '[':  // 0x5B
      case ']':  // 0x5D
      case '{':  // 0x7B
      case '}':  // 0x7D
        throw read_error(make<string>("left and right square and curly brackets (braces) are reserved for possible future extensions to the language"),
                         make<character>(c1));

      default:
        if (auto const& token = take_token(); token == ".")
        {
          throw std::integral_constant<char, '.'>();
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

  auto textual_input_port::read_character_literal() -> character
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

    auto & is = static_cast<std::istream &>(*this);

    if (auto buffer = std::string(2, '\0'); not is.read(buffer.data(), buffer.size()) or buffer != "#\\")
    {
      throw read_error(make<string>("not a character"),
                       make<string>(buffer));
    }
    else switch (auto token = take_token(); token.length())
    {
    case 0:
      assert(is_special_character(is.peek()));
      return character(is.get());

    case 1:
      assert(std::isprint(token.front()));
      return character(token.front());

    default:
      if (auto iter = names.find(token); iter != std::end(names))
      {
        return character(iter->second);
      }
      else if (token[0] == 'x' and 1 < token.length())
      {
        return character(lexical_cast<character::int_type>(std::hex, token.substr(1)));
      }
      else
      {
        throw read_error(make<string>("not a character"),
                         make<string>("\\#" + token));
      }
    }
  }

  auto textual_input_port::read_string_literal() -> string
  {
    auto s = string();

    auto & is = static_cast<std::istream &>(*this);

    auto const quotation_mark = is.get();

    for (auto codepoint = take_codepoint(); not character::is_eof(codepoint); codepoint = take_codepoint())
    {
      if (codepoint == quotation_mark)
      {
        return s;
      }
      else switch (codepoint)
      {
      case '\\':
        switch (auto const codepoint = take_codepoint(); codepoint)
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
  }

  auto textual_input_port::take_codepoint() -> character::int_type
  {
    /*
       00000000 -- 0000007F: 0xxxxxxx
       00000080 -- 000007FF: 110xxxxx 10xxxxxx
       00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
       00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    */

    character::int_type codepoint = 0;

    auto & istream = static_cast<std::istream &>(*this);

    if (auto const c = istream.peek(); character::is_eof(c))
    {
      return character::eof();
    }
    else if (0x00 <= c and c <= 0x7F) // 7 bit
    {
      codepoint = istream.get();
    }
    else if (0xC2 <= c and c <= 0xDF) // 11 bit
    {
      codepoint |= istream.get() bitand 0b0001'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111;
    }
    else if (0xE0 <= c and c <= 0xEF) // 16 bit
    {
      codepoint |= istream.get() bitand 0b0000'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111;
    }
    else if (0xF0 <= c and c <= 0xF4) // 21 bit
    {
      codepoint |= istream.get() bitand 0b0000'0111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= istream.get() bitand 0b0011'1111;
    }
    else
    {
      throw read_error(make<string>("an end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable"));
    }

    return codepoint;
  }

  auto textual_input_port::take_digits() -> std::string
  {
    auto s = std::string();

    for (auto & istream = static_cast<std::istream &>(*this);
         std::isdigit(istream.peek());
         s.push_back(istream.get()))
    {}

    return s.length() ? s : "0";
  }

  auto textual_input_port::take_nested_block_comment() -> void
  {
    auto & istream = static_cast<std::istream &>(*this);

    while (not character::is_eof(istream.peek()))
    {
      switch (istream.get())
      {
      case '#':
        switch (istream.peek())
        {
        case '|':
          istream.ignore(1);
          take_nested_block_comment();
          [[fallthrough]];

        default:
          continue;
        }

      case '|':
        switch (istream.peek())
        {
        case '#':
          istream.ignore(1);
          return;

        default:
          continue;
        }

      default:
        continue;
      }
    }

    throw read_error(make<string>("An end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable"));
  }

  auto textual_input_port::take_token() -> std::string
  {
    auto token = std::string();

    for (auto & istream = static_cast<std::istream &>(*this);
         not is_special_character(istream.peek());
         token.push_back(fold_case ? std::tolower(istream.get()) : istream.get()))
    {}

    return token;
  }
} // namespace kernel
} // namespace meevax
