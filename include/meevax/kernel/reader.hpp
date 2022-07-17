/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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
  auto read_codepoint = [](std::istream & is) /* -------------------------------
  *
  *  00000000 -- 0000007F: 0xxxxxxx
  *  00000080 -- 000007FF: 110xxxxx 10xxxxxx
  *  00000800 -- 0000FFFF: 1110xxxx 10xxxxxx 10xxxxxx
  *  00010000 -- 001FFFFF: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
  *
  * ------------------------------------------------------------------------- */
  {
    character::int_type codepoint = 0;

    if (auto const c = is.peek(); std::char_traits<char>::eq(std::char_traits<char>::eof(), c))
    {
      throw eof();
    }
    else if (0x00 <= c and c <= 0x7F) // 7 bit
    {
      codepoint = is.get();
    }
    else if (0xC2 <= c and c <= 0xDF) // 11 bit
    {
      codepoint |= is.get() bitand 0b0001'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111;
    }
    else if (0xE0 <= c and c <= 0xEF) // 16 bit
    {
      codepoint |= is.get() bitand 0b0000'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111;
    }
    else if (0xF0 <= c and c <= 0xF4) // 21 bit
    {
      codepoint |= is.get() bitand 0b0000'0111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111; codepoint <<= 6;
      codepoint |= is.get() bitand 0b0011'1111;
    }
    else
    {
      throw read_error(make<string>("invalid stream"), unit);
    }

    return codepoint;
  };

  template <typename Environment>
  class reader
  {
    friend Environment;

    explicit reader()
    {}

    IMPORT(Environment, evaluate, NIL);

    using char_type = typename std::istream::char_type;

  public:
    static inline std::unordered_map<external_representation, value_type> symbols {};

    inline auto char_ready() const
    {
      return standard_input.is_also<std::istream>() and standard_input.as<std::istream>();
    }

    static auto make_number(external_representation const& token, int radix = 10)
    {
      try
      {
        return make<exact_integer>(token, radix);
      }
      catch (...)
      {
        try
        {
          return ratio(token, radix).simple();
        }
        catch (...)
        {
          try
          {
            return make<double_float>(token);
          }
          catch (...)
          {
            if (auto iter = constants.find(token); iter != std::end(constants))
            {
              return iter->second;
            }
            else
            {
              throw read_error(make<string>("not a number"), make<string>(token));
            }
          }
        }
      }
    }

    static auto make_symbol(external_representation const& name) -> const_reference
    {
      if (auto const iter = symbols.find(name); iter != std::end(symbols))
      {
        return iter->second;
      }
      else if (auto const [iter, success] = symbols.emplace(name, make<symbol>(name)); success)
      {
        return iter->second;
      }
      else
      {
        throw error(make<string>("failed to intern a symbol"), make<string>(name));
      }
    }

    inline auto read(std::istream & is) -> value_type
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
            return cons(kar, read(is.putback(c)));
          }
          catch (std::integral_constant<char_type, ')'> const&) { return std::char_traits<char_type>::eq(c, '(') ? unit : throw; }
          catch (std::integral_constant<char_type, ']'> const&) { return std::char_traits<char_type>::eq(c, '[') ? unit : throw; }
          catch (std::integral_constant<char_type, '}'> const&) { return std::char_traits<char_type>::eq(c, '{') ? unit : throw; }
          catch (std::integral_constant<char_type, '.'> const&)
          {
            let const kdr = read(is);

            switch (c)
            {
            case '(': is.ignore(std::numeric_limits<std::streamsize>::max(), ')'); break;
            case '[': is.ignore(std::numeric_limits<std::streamsize>::max(), ']'); break;
            case '{': is.ignore(std::numeric_limits<std::streamsize>::max(), '}'); break;
            }

            return kdr;
          }

        case ')': throw std::integral_constant<char_type, ')'>();
        case ']': throw std::integral_constant<char_type, ']'>();
        case '}': throw std::integral_constant<char_type, '}'>();

        case '"':
          return make<string>(is);

        case '\'':
          return list(make_symbol("quote"), read(is));

        case '`':
          return list(make_symbol("quasiquote"), read(is));

        case ',':
          switch (is.peek())
          {
          case '@':
            is.ignore(1);
            return list(make_symbol("unquote-splicing"), read(is));

          default:
            return list(make_symbol("unquote"), read(is));
          }

        case '#':
          switch (auto const c = is.get())
          {
          case '!': // from SRFI 22
            is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            return read(is);

          case ',': // from SRFI 10
            return evaluate(read(is));

          case ';': // from SRFI 62
            return read(is), read(is);

          case 'b': // (string->number (read) 2)
            return make_number(is.peek() == '#' ? lexical_cast<external_representation>(read(is)) : read_token(is), 2);

          case 'c': // from Common Lisp
            {
              let const xs = read(is);
              return make<complex>(list_tail(xs, 0).is<pair>() ? list_ref(xs, 0) : e0,
                                   list_tail(xs, 1).is<pair>() ? list_ref(xs, 1) : e0);
            }

          case 'd':
            return make_number(is.peek() == '#' ? lexical_cast<external_representation>(read(is)) : read_token(is), 10);

          case 'e':
            return read(is).template as<number>().exact(); // NOTE: Same as #,(exact (read))

          case 'f':
            read_token(is);
            return f;

          case 'i':
            return read(is).template as<number>().inexact(); // NOTE: Same as #,(inexact (read))

          case 'o':
            return make_number(is.peek() == '#' ? lexical_cast<external_representation>(read(is)) : read_token(is), 8);

          case 't':
            read_token(is);
            return t;

          case 'x':
            return make_number(is.peek() == '#' ? lexical_cast<external_representation>(read(is)) : read_token(is), 16);

          case '(':
            is.putback(c);
            return make<vector>(read(is));

          case '\\':
            return read_character(is);

          default:
            throw read_error(make<string>("unknown discriminator"), make<character>(c));
          }

        default:
          if (auto const token = read_token(is.putback(c)); token == ".")
          {
            throw std::integral_constant<char_type, '.'>();
          }
          else try
          {
            return make_number(token, 10);
          }
          catch (...)
          {
            return make_symbol(token);
          }
        }
      }

      return eof_object;
    }

    inline auto read(const_reference x) -> decltype(auto)
    {
      return read(x.as<std::istream>());
    }

    inline auto read() -> decltype(auto)
    {
      return read(standard_input);
    }

    inline auto read(external_representation const& s) -> value_type // NOTE: Specifying `decltype(auto)` causes a `undefined reference to ...` error in GCC-7.
    {
      auto port = std::stringstream(s);
      return read(port);
    }

    static auto read_character(std::istream & is)
    {
      std::unordered_map<external_representation, character::int_type> static const character_names {
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

      switch (auto token = read_token(is); token.length())
      {
      case 0:
        return make<character>(is.get());

      case 1:
        return make<character>(token[0]);

      default:
        if (auto iter = character_names.find(token); iter != std::end(character_names))
        {
          return make<character>(iter->second);
        }
        else if (token[0] == 'x' and 1 < token.length())
        {
          std::stringstream ss;
          ss << std::hex << token.substr(1);

          character::int_type value = 0;
          ss >> value;

          return make<character>(value);
        }
        else
        {
          putback(is, token);
          throw read_error(make<string>("not a character"), make<string>("\\#" + token));
        }
      }
    }

    static auto read_token(std::istream & is) -> external_representation
    {
      auto is_end = [](auto c) constexpr
      {
        auto one_of = [c](auto... xs) constexpr
        {
          return (std::char_traits<char>::eq(c, xs) or ...);
        };

        return std::isspace(c) or one_of('"', '#', '\'', '(', ')', ',', ';', '[', ']', '`', '{', '|', '}', std::char_traits<char>::eof()); // NOTE: What read treats specially.
      };

      external_representation token;

      for (auto c = is.peek(); not is_end(c); c = is.peek())
      {
        token.push_back(is.get());
      }

      return token;
    };
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP
