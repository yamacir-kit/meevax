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

#include <cctype>
#include <meevax/iostream/combinator.hpp>
#include <meevax/iostream/ignore.hpp>
#include <meevax/iostream/putback.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/miscellaneous.hpp> // for eof
#include <meevax/kernel/numeric_io.hpp>
#include <meevax/kernel/parser.hpp> // DEPRECATED
#include <meevax/kernel/port.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  namespace parse
  {
    using meevax::iostream::operator |;
    using meevax::iostream::operator +;

    auto empty = [](std::istream &)
    {
      return static_cast<std::string>("");
    };

    auto one_of = [](auto... xs)
    {
      return [=](std::istream & is)
      {
        if (auto c = static_cast<character>(is.get()); ((c == xs) or ...))
        {
          return static_cast<std::string>(c);
        }
        else
        {
          throw read_error(make<string>("unexpected character"), make<character>(c));
        }
      };
    };

    auto none_of = [](auto... xs)
    {
      return [=](std::istream & is)
      {
        if (auto c = static_cast<character>(is.get()); ((c != xs) or ...))
        {
          return static_cast<std::string>(c);
        }
        else
        {
          throw read_error(make<string>("unexpected character"), make<character>(c));
        }
      };
    };

    auto sequence = [](std::string const& s)
    {
      return [=](std::istream & is)
      {
        for (auto c : s)
        {
          one_of(c)(is);
        }

        return s;
      };
    };

    auto satisfy = [](auto&& f)
    {
      return [=](std::istream & is)
      {
        if (auto c = static_cast<character>(is.get()); f(c))
        {
          return static_cast<std::string>(c);
        }
        else
        {
          is.putback(c);
          read_error(make<string>("unexpected character"), make<character>(c));
        }
      };
    };

    auto intraline_whitespace = one_of(' ', '\t');

    auto line_ending = one_of('\n', '\r');

    auto whitespace = intraline_whitespace | line_ending;

    auto vertical_line = one_of('|');

    auto delimiter = whitespace | vertical_line | one_of('(', ')', '"', ';');

    auto upper = satisfy([](auto c) { return std::isupper(c); });

    auto lower = satisfy([](auto c) { return std::islower(c); });

    auto letter = upper | lower;

    auto special_initial = one_of('!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~');

    auto initial = letter | special_initial;

    auto digit = satisfy([](auto c) { return std::isdigit(c); });

    auto explicit_sign = one_of('+', '-');

    auto special_subsequent = explicit_sign | one_of('.', '@');

    auto subsequent = initial | digit | special_subsequent;

    // TODO auto any_character_other_than_vertical_line_or_backslash

    // TODO auto inline_hex_escape

    // TODO auto mnemonic_escape

    auto symbol_element = letter;
                        //   any_character_other_than_vertical_line_or_backslash
                        // | inline_hex_escape
                        // | mnemonic_escape
                        // | s("\\|")

    auto sign_subsequent = initial | explicit_sign | one_of('@');

    auto dot_subsequent = sign_subsequent | one_of('.');

    auto peculiar_identifier = explicit_sign
                             | explicit_sign + sign_subsequent + many(subsequent)
                             | explicit_sign + one_of('.') + dot_subsequent + many(subsequent)
                             | one_of('.') + dot_subsequent + many(subsequent);

    // TODO auto boolean

    // TODO auto number

    // TODO auto character

    // TODO auto string

    auto identifier = initial + many(subsequent)
                    | vertical_line + many(symbol_element) + vertical_line
                    | peculiar_identifier;

    auto token = [](std::istream & is) //  = <identifier> | <boolean> | <number> | <character> | <string> | ( | ) | #( | #u8( | ’ | ` | , | ,@ | .
    {
      std::string result;

      for (auto c = is.peek(); not is_end_of_token(c); c = is.peek())
      {
        result.push_back(is.get());
      }

      return result;
    };

    auto any_character = [](std::istream & is)
    {
      switch (auto s = token(is); std::size(s))
      {
      case 0:
        return make<character>(is.get());

      case 1:
        return make<character>(s[0]);

      default:
        putback(is, s);
        throw tagged_read_error<character>(
          make<string>("If <character> in #\\<character> is alphabetic, then any character immediately following <character> cannot be one that can appear in an identifier"), unit);
      }
    };

    auto character_name = [](std::istream & is)
    {
      std::unordered_map<std::string, character::value_type> static const character_names
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

      auto const name = token(is);

      try
      {
        return make<character>(character_names.at(name));
      }
      catch (...)
      {
        putback(is, name);
        throw tagged_read_error<character>(make<string>("invalid <charcter name>"), make<string>("\\#" + name));
      }
    };

    auto hex_scalar_value = [](std::istream & is)
    {
      if (auto s = token(is); s[0] == 'x' and 1 < std::size(s))
      {
        std::stringstream ss;
        ss << std::hex << s.substr(1);

        character::value_type value = 0;
        ss >> value;

        return make<character>(value);
      }
      else
      {
        putback(is, s);
        throw tagged_read_error<character>(make<string>("invalid <hex scalar value>"), make<string>("\\#" + s));
      }
    };

    auto character = any_character | character_name | hex_scalar_value;
  }

  auto comment = [](std::istream & is)
  {
    is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
  };

  // ---------------------------------------------------------------------------

  // auto read_char(std::istream &) -> pair::value_type;

  template <typename Module>
  class reader
  {
    friend Module;

    explicit reader()
    {}

    IMPORT(Module, evaluate,            NIL);
    IMPORT(Module, standard_debug_port, NIL);
    IMPORT(Module, write_to,            NIL);

    using char_type = typename std::istream::char_type;

    template <char_type C>
    using char_constant = std::integral_constant<char_type, C>;

  public:
    static inline std::unordered_map<std::string, pair::value_type> symbols {};

    inline auto char_ready() const
    {
      return default_input_port.is_polymorphically<std::istream>() and default_input_port.as<std::istream>();
    }

    static auto intern(std::string const& name) -> pair::const_reference
    {
      if (auto const iter = symbols.find(name); iter != std::end(symbols))
      {
        return std::get<1>(*iter);
      }
      else if (auto const [iter, success] = symbols.emplace(name, make<symbol>(name)); success)
      {
        return std::get<1>(*iter);
      }
      else
      {
        throw error(make<string>("failed to intern a symbol"), make<string>(name));
      }
    }

    inline auto read(std::istream & is) -> pair::value_type
    {
      std::string buffer {};

      for (auto head = std::istream_iterator<char_type>(is); head != std::istream_iterator<char_type>(); ++head)
      {
        switch (auto const c = *head)
        {
        case ';':
          comment(is);
          break;

        case ' ':
        case '\f':
        case '\n':
        case '\r':
        case '\t':
        case '\v':
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
          catch (tagged_read_error<char_constant<')'>> const&) { return std::char_traits<char_type>::eq(c, '(') ? unit : throw; }
          catch (tagged_read_error<char_constant<']'>> const&) { return std::char_traits<char_type>::eq(c, '[') ? unit : throw; }
          catch (tagged_read_error<char_constant<'}'>> const&) { return std::char_traits<char_type>::eq(c, '{') ? unit : throw; }
          catch (tagged_read_error<char_constant<'.'>> const&)
          {
            let const kdr = read(is);

            switch (c)
            {
            case '(': ignore(is, [](auto c) { return not std::char_traits<char_type>::eq(c, ')'); }).get(); break;
            case '[': ignore(is, [](auto c) { return not std::char_traits<char_type>::eq(c, ']'); }).get(); break;
            case '{': ignore(is, [](auto c) { return not std::char_traits<char_type>::eq(c, '}'); }).get(); break;
            }

            return kdr;
          }

        case ')':
          throw tagged_read_error<char_constant<')'>>(make<string>("unexpected character"), make<character>(c));

        case ']':
          throw tagged_read_error<char_constant<']'>>(make<string>("unexpected character"), make<character>(c));

        case '}':
          throw tagged_read_error<char_constant<'}'>>(make<string>("unexpected character"), make<character>(c));

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

        case '#':
          switch (auto const discriminator = is.get())
          {
          case '!': // from SRFI-22
            comment(is);
            return read(is);

          case ',': // from SRFI-10
            return evaluate(read(is));

          case ';': // from SRFI-62
            return read(is), read(is);

          case 'b': // (string->number (read) 2)
            return to_number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : parse::token(is), 2);

          case 'c': // from Common Lisp
            if (let const xs = read(is); xs.is<null>())
            {
              return make<complex>(e0, e0);
            }
            else if (not cdr(xs).is<pair>())
            {
              return make<complex>(car(xs), e0);
            }
            else
            {
              return make<complex>(car(xs), cadr(xs));
            }

          case 'd':
            return to_number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : parse::token(is), 10);

          case 'e':
            return exact(read(is)); // NOTE: Same as #,(exact (read))

          case 'f':
            ignore(is, [](auto&& x) { return not is_end_of_token(x); });
            return f;

          case 'i':
            return inexact(read(is)); // NOTE: Same as #,(inexact (read))

          case 'o':
            return to_number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : parse::token(is), 8);

          case 'p':
            assert(is.get() == '"');
            is.ignore(1);
            return make<path>(string(is));

          case 't':
            ignore(is, [](auto&& x) { return not is_end_of_token(x); });
            return t;

          case 'x':
            return to_number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : parse::token(is), 16);

          case '(':
            is.putback(discriminator);
            return make<vector>(for_each_in, read(is));

          case '\\':
            return parse::character(is);

          default:
            throw read_error(make<string>("unknown discriminator"), make<character>(discriminator));
          }

        default:
          if (buffer.push_back(c); is_end_of_token(is.peek()))
          {
            if (buffer == ".")
            {
              throw tagged_read_error<char_constant<'.'>>(make<string>("unexpected character: "), make<character>('.'));
            }
            else try
            {
              return to_number(buffer, 10);
            }
            catch (...)
            {
              return intern(buffer);
            }
          }
        }
      }

      return eof_object;
    }

    inline auto read(std::istream && is)
    {
      return read(is);
    }

    inline auto read(pair::const_reference x) -> pair::value_type
    {
      if (x.is_polymorphically<std::istream>())
      {
        return read(x.as<std::istream>());
      }
      else
      {
        throw read_error(make<string>("not an input-port"), x);
      }
    }

    inline auto read() -> pair::value_type
    {
      let const result = read(default_input_port);

      write_to(standard_debug_port(), "\n", header(__func__), result, "\n");

      return result;
    }

    inline auto read(std::string const& s) -> pair::value_type
    {
      return read(std::stringstream(s));
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP
