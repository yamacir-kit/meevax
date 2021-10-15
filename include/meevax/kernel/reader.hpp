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

#include <meevax/iostream/combinator.hpp>
#include <meevax/iostream/ignore.hpp>
#include <meevax/iostream/putback.hpp>
#include <meevax/kernel/constant.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/miscellaneous.hpp> // for eof
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  namespace parse
  {
    // using meevax::iostream::operator *;
    // using meevax::iostream::operator +;
    using meevax::iostream::operator |;

    // auto intraline_whitespace = satisfy([](auto c) { return std::isblank(c); });

    // auto line_ending = sequence("\r\n") | one_of('\n', '\r');

    // auto whitespace = intraline_whitespace | line_ending;

    // auto vertical_line = one_of('|');

    // auto delimiter = whitespace | vertical_line | one_of('(', ')', '"', ';');

    // auto letter = satisfy([](auto c) { return std::isalpha(c); });

    // auto special_initial = one_of('!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~');

    // auto initial = letter | special_initial;

    // auto digit = satisfy([](auto c) { return std::isdigit(c); });

    // auto hex_digit = satisfy([](auto c) { return std::isxdigit(c); });

    // auto explicit_sign = one_of('+', '-');

    // auto special_subsequent = explicit_sign | one_of('.', '@');

    // auto subsequent = initial | digit | special_subsequent;

    // auto inline_hex_escape = sequence("\\x") + hex_digit + many(hex_digit);

    // TODO auto any_character_other_than_vertical_line_or_backslash

    // auto symbol_element = letter;
    //                     //   any_character_other_than_vertical_line_or_backslash
    //                     // | inline_hex_escape
    //                     // | mnemonic_escape
    //                     // | s("\\|")

    // auto sign_subsequent = initial | explicit_sign | one_of('@');

    // auto dot_subsequent = sign_subsequent | one_of('.');

    // auto peculiar_identifier = explicit_sign
    //                          | explicit_sign + sign_subsequent + many(subsequent)
    //                          | explicit_sign + one_of('.') + dot_subsequent + many(subsequent)
    //                          | one_of('.') + dot_subsequent + many(subsequent);

    // auto identifier = initial + many(subsequent)
    //                 | vertical_line + many(symbol_element) + vertical_line
    //                 | peculiar_identifier;

    // auto boolean = sequence("#true") | sequence("#t") | sequence("#false") | sequence("#f");

    auto token = [](std::istream & is) //  = <identifier> | <boolean> | <number> | <character> | <string> | ( | ) | #( | #u8( | ’ | ` | , | ,@ | .
    {
      auto is_end = [](auto c) constexpr
      {
        auto one_of = [c](auto... xs) constexpr
        {
          return (std::char_traits<char>::eq(c, xs) or ...);
        };

        return std::isspace(c) or one_of('"', '#', '\'', '(', ')', ',', ';', '[', ']', '`', '{', '|', '}', std::char_traits<char>::eof()); // NOTE: What read treats specially.
      };

      std::string result;

      for (auto c = is.peek(); not is_end(c); c = is.peek())
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
        throw read_error(make<string>("If <character> in #\\<character> is alphabetic, then any character immediately following <character> cannot be one that can appear in an identifier"));
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
        throw read_error(make<string>("invalid <charcter name>"), make<string>("\\#" + name));
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
        throw read_error(make<string>("invalid <hex scalar value>"), make<string>("\\#" + s));
      }
    };

    auto character = any_character | character_name | hex_scalar_value;
  }

  namespace string_to
  {
    template <typename F, typename G, REQUIRES(std::is_invocable<F, std::string const&, int>,
                                               std::is_invocable<G, std::string const&, int>)>
    auto operator |(F&& f, G&& g)
    {
      return [=](std::string const& token, auto radix)
      {
        try
        {
          return f(token, radix);
        }
        catch (...)
        {
          return g(token, radix);
        }
      };
    }

    auto integer = [](std::string const& token, auto radix = 10)
    {
      auto const result = exact_integer(token, radix);
      return make(result);
    };

    auto ratio = [](std::string const& token, auto radix = 10)
    {
      if (auto const value = meevax::ratio(token, radix).reduce(); value.is_integer())
      {
        return std::get<0>(value);
      }
      else
      {
        return make(value);
      }
    };

    auto decimal = [](std::string const& token, auto)
    {
      auto const result = double_float(token);
      return make(result);
    };

    auto flonum = [](std::string const& token, auto)
    {
      if (auto iter = constants.find(token); iter != std::end(constants))
      {
        return std::get<1>(*iter);
      }
      else
      {
        throw read_error(make<string>("not a number"), make<string>(token));
      }
    };

    auto real = integer | ratio | decimal | flonum;

    auto complex = real;

    auto number = complex;
  } // namespace string_to

  template <typename EnvironmentSpecifier>
  class reader
  {
    friend EnvironmentSpecifier;

    explicit reader()
    {}

    IMPORT(EnvironmentSpecifier, evaluate,   NIL);
    IMPORT(EnvironmentSpecifier, debug_port, NIL);
    IMPORT(EnvironmentSpecifier, write,      NIL);

    using char_type = typename std::istream::char_type;

  public:
    static inline std::unordered_map<std::string, pair::value_type> symbols {};

    inline auto char_ready() const
    {
      return standard_input.is_also<std::istream>() and standard_input.as<std::istream>();
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
      for (auto head = std::istream_iterator<char_type>(is); head != std::istream_iterator<char_type>(); ++head)
      {
        switch (auto const c = *head)
        {
        case ';':
          is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
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
          catch (std::integral_constant<char_type, ')'> const&) { return std::char_traits<char_type>::eq(c, '(') ? unit : throw; }
          catch (std::integral_constant<char_type, ']'> const&) { return std::char_traits<char_type>::eq(c, '[') ? unit : throw; }
          catch (std::integral_constant<char_type, '}'> const&) { return std::char_traits<char_type>::eq(c, '{') ? unit : throw; }
          catch (std::integral_constant<char_type, '.'> const&)
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

        case ')': throw std::integral_constant<char_type, ')'>();
        case ']': throw std::integral_constant<char_type, ']'>();
        case '}': throw std::integral_constant<char_type, '}'>();

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
          switch (auto const c = is.get())
          {
          case '!': // from SRFI-22
            is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            return read(is);

          case ',': // from SRFI-10
            return evaluate(read(is));

          case ';': // from SRFI-62
            return read(is), read(is);

          case 'b': // (string->number (read) 2)
            return string_to::number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : parse::token(is), 2);

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
            return string_to::number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : parse::token(is), 10);

          case 'e':
            return read(is).exact(); // NOTE: Same as #,(exact (read))

          case 'f':
            parse::token(is);
            return f;

          case 'i':
            return read(is).inexact(); // NOTE: Same as #,(inexact (read))

          case 'o':
            return string_to::number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : parse::token(is), 8);

          case 't':
            parse::token(is);
            return t;

          case 'x':
            return string_to::number(is.peek() == '#' ? lexical_cast<std::string>(read(is)) : parse::token(is), 16);

          case '(':
            is.putback(c);
            return make<vector>(for_each_in, read(is));

          case '\\':
            return parse::character(is);

          default:
            throw read_error(make<string>("unknown discriminator"), make<character>(c));
          }

        default:
          if (auto const token = c + parse::token(is); token == ".")
          {
            throw std::integral_constant<char_type, '.'>();
          }
          else try
          {
            return string_to::number(token, 10);
          }
          catch (...)
          {
            return intern(token);
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
      if (x.is_also<std::istream>())
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
      let const result = read(standard_input);

      write(debug_port(), header(__func__), result, "\n");

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
