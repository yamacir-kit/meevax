/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
#include <meevax/kernel/homogeneous_vector.hpp>
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/textual_input_port.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  textual_input_port::iterator::iterator(textual_input_port & input)
    : input { std::addressof(input) }
    , value { input.read() }
  {}

  auto textual_input_port::iterator::operator *() -> reference
  {
    return value;
  }

  auto textual_input_port::iterator::operator ->() -> pointer
  {
    return &value;
  }

  auto textual_input_port::iterator::operator ++() -> iterator &
  {
    if (input)
    {
      value = input->read();
    }

    return *this;
  }

  auto textual_input_port::iterator::operator ++(int) -> iterator
  {
    auto copy = *this;
    operator ++();
    return copy;
  }

  auto operator ==(textual_input_port::iterator const& a,
                   textual_input_port::iterator const& b) -> bool
  {
    return eqv(a.value, b.value);
  }

  auto operator !=(textual_input_port::iterator const& a,
                   textual_input_port::iterator const& b) -> bool
  {
    return not (a == b);
  }

  struct datum_label
  {
    std::string const n;

    template <typename... Ts>
    explicit datum_label(Ts&&... xs)
      : n { std::forward<decltype(xs)>(xs)... }
    {}
  };

  auto circulate(object & xs, object const& x, std::string const& n) -> void
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

  auto circulate(object & xs, std::string const& n) -> void
  {
    return circulate(xs, xs, n);
  }

  auto textual_input_port::begin() -> iterator
  {
    return iterator(*this);
  }

  auto textual_input_port::end() -> iterator
  {
    return iterator();
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
    if (character::is_eof(istream().peek()))
    {
      return eof_object;
    }
    else
    {
      auto s = string();

      while (size-- and not character::is_eof(istream().peek()))
      {
        s.vector.emplace_back(take_codepoint());
      }

      return make(s);
    }
  }

  auto textual_input_port::get_line() -> object
  {
    if (auto s = std::string(); std::getline(istream(), s).eof())
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
    return static_cast<bool>(istream());
  }

  auto textual_input_port::good() const -> bool
  {
    return istream().good();
  }

  auto textual_input_port::ignore(std::size_t size) -> textual_input_port &
  {
    while (size-- and not character::is_eof(peek_codepoint()))
    {
      take_codepoint();
    }

    return *this;
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
    auto c = take_codepoint();

    auto s = static_cast<std::string>(character(c));

    for (auto iter = std::rbegin(s); iter != std::rend(s); ++iter)
    {
      istream().putback(*iter);
    }

    return c;
  }

  auto textual_input_port::read() -> object
  {
    while (not character::is_eof(istream().peek()))
    {
      switch (auto const c1 = peek_codepoint())
      {
      case '\t': // 0x09
      case '\n': // 0x0A
      case '\v': // 0x0B
      case '\f': // 0x0C
      case '\r': // 0x0D
      case ' ':  // 0x20
        ignore(1);
        break;

      case '"':  // 0x22
        return make(read_string_literal());

      case '#':  // 0x23
        switch (auto const c2 = ignore(1).peek_codepoint())
        {
        case '!': // SRFI 22
          if (auto token = take_token(); token == "!fold-case")
          {
            case_sensitive = false;
          }
          else if (token == "!no-fold-case")
          {
            case_sensitive = true;
          }
          else
          {
            istream().ignore(std::numeric_limits<std::streamsize>::max(), '\n');
          }

          return read();

        case ',': // SRFI 10
          return interaction_environment().as<environment>().evaluate(ignore(1).read());

        case ';': // SRFI 62
          ignore(1).read(); // Discard an expression.
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
          switch (auto label = take_digits(); peek_codepoint())
          {
          case '#':
            ignore(1);

            if (auto iter = datum_labels.find(label); iter != datum_labels.end())
            {
              return iter->second;
            }
            else
            {
              throw read_error(make<string>("it is an error to attempt a forward reference"),
                               make<string>(lexical_cast<std::string>('#', label, '#')));
            }

          case '=':
            ignore(1);

            if (auto [iter, success] = datum_labels.emplace(label, make<datum_label>(label)); success)
            {
              if (let xs = read(); xs != iter->second)
              {
                circulate(xs, label);
                datum_labels.erase(label);
                return xs;
              }
              else
              {
                return nullptr;
              }
            }
            else
            {
              throw read_error(make<string>("duplicated datum-label declaration"),
                               make<string>(label));
            }

          default:
            throw read_error(make<string>("unknown discriminator"),
                             make<string>(lexical_cast<std::string>('#', label, get())));
          }

        case 'b':
          return make_number(ignore(1).peek_codepoint() == '#' ? lexical_cast<std::string>(read()) : take_token(), 2);

        case 'd':
          return make_number(ignore(1).peek_codepoint() == '#' ? lexical_cast<std::string>(read()) : take_token(), 10);

        case 'e':
          return exact(ignore(1).read()); // NOTE: Same as #,(exact (read))

        case 'f':
          switch (auto const digits = ignore(1).take_digits(); std::stoi(digits))
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
          return inexact(ignore(1).read()); // NOTE: Same as #,(inexact (read))

        case 'o':
          return make_number(ignore(1).peek_codepoint() == '#' ? lexical_cast<std::string>(read()) : take_token(), 8);

        case 's':
          switch (auto const digits = ignore(1).take_digits(); std::stoi(digits))
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
          switch (auto const digits = ignore(1).take_digits(); std::stoi(digits))
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
          return make_number(ignore(1).peek_codepoint() == '#' ? lexical_cast<std::string>(read()) : take_token(), 16);

        case '(':
          return make<vector>(read());

        case '\\':
          istream().putback(c1);
          return make(read_character_literal());

        case '|': // SRFI 30
          ignore(1).take_nested_block_comment();
          return read();

        default:
          throw read_error(make<string>("unknown discriminator"), make<character>(c2));
        }

      case '\'': // 0x27
        return list(make_symbol("quote"), ignore(1).read());

      case '(':  // 0x28
        try
        {
          if (let const& x = ignore(1).read(); x.is<eof>())
          {
            return x;
          }
          else
          {
            istream().putback('('); // modifying putback (https://en.cppreference.com/w/cpp/io/basic_istream/putback)
            return cons(x, read());
          }
        }
        catch (std::integral_constant<char, ')'> const&)
        {
          return nullptr;
        }
        catch (std::integral_constant<char, '.'> const&)
        {
          let const x = read();
          istream().ignore(std::numeric_limits<std::streamsize>::max(), ')');
          return x;
        }

      case ')':  // 0x29
        ignore(1);
        throw std::integral_constant<char, ')'>();

      case ',':  // 0x2C
        switch (ignore(1).peek_codepoint())
        {
        case '@':
          return list(make_symbol("unquote-splicing"), ignore(1).read());

        default:
          return list(make_symbol("unquote"), read());
        }

      case ';':  // 0x3B
        istream().ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        break;

      case '`':  // 0x60
        return list(make_symbol("quasiquote"), ignore(1).read());

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
    ignore(2); // sharp and backslash

    if (auto c = take_codepoint(); is_special_character(peek_codepoint())) // #\<character>
    {
      return character(c);
    }
    else if (c == 'x') // #\x<hex scalar value>
    {
      return character(lexical_cast<character::int_type>(std::hex, take_token()));
    }
    else // #\<character name>
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

      auto name = static_cast<std::string>(character(c)) + take_token();

      if (auto iter = names.find(name); iter != names.end())
      {
        return character(iter->second);
      }
      else
      {
        throw read_error(make<string>("unknown character name"),
                         make<string>("\\#" + name));
      }
    }
  }

  auto textual_input_port::read_string_literal() -> string
  {
    auto s = string();

    auto const quotation_mark = take_codepoint();

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
        case 'a': s.vector.emplace_back('\a'); break;
        case 'b': s.vector.emplace_back('\b'); break;
        case 'f': s.vector.emplace_back('\f'); break;
        case 'n': s.vector.emplace_back('\n'); break;
        case 'r': s.vector.emplace_back('\r'); break;
        case 't': s.vector.emplace_back('\t'); break;
        case 'v': s.vector.emplace_back('\v'); break;
        case 'x':
          if (auto token = std::string(); std::getline(istream(), token, ';'))
          {
            s.vector.emplace_back(lexical_cast<character::int_type>(std::hex, token));
          }
          break;

        case '\n':
        case '\r':
          while (std::isspace(peek_codepoint()))
          {
            ignore(1);
          }
          break;

        default:
          s.vector.emplace_back(codepoint);
          break;
        }
        break;

      default:
        s.vector.emplace_back(codepoint);
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

    if (auto const c = istream().peek(); character::is_eof(c) or character::is_ascii(c))
    {
      return istream().get();
    }
    else if (0xC2 <= c and c <= 0xDF) // 11 bit
    {
      return (istream().get() & 0b0001'1111) << 6
           | (istream().get() & 0b0011'1111);
    }
    else if (0xE0 <= c and c <= 0xEF) // 16 bit
    {
      return (istream().get() & 0b0000'1111) << 12
           | (istream().get() & 0b0011'1111) <<  6
           | (istream().get() & 0b0011'1111);
    }
    else if (0xF0 <= c and c <= 0xF4) // 21 bit
    {
      return (istream().get() & 0b0000'0111) << 18
           | (istream().get() & 0b0011'1111) << 12
           | (istream().get() & 0b0011'1111) <<  6
           | (istream().get() & 0b0011'1111);
    }
    else
    {
      throw read_error(make<string>("an end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable"));
    }
  }

  auto textual_input_port::take_digits() -> std::string
  {
    auto s = std::string();

    while (std::isdigit(istream().peek()))
    {
      s.push_back(istream().get());
    }

    return s.length() ? s : "0";
  }

  auto textual_input_port::take_nested_block_comment() -> void
  {
    while (not character::is_eof(istream().peek()))
    {
      switch (istream().get())
      {
      case '#':
        switch (istream().peek())
        {
        case '|':
          istream().ignore(1);
          take_nested_block_comment();
          [[fallthrough]];

        default:
          continue;
        }

      case '|':
        switch (istream().peek())
        {
        case '#':
          istream().ignore(1);
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

    while (not is_special_character(istream().peek()))
    {
      token.push_back(case_sensitive ? istream().get() : std::tolower(istream().get()));
    }

    return token;
  }
} // namespace kernel
} // namespace meevax
