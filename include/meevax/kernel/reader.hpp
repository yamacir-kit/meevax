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
  auto get_codepoint(std::istream &) -> character::int_type;

  auto get_delimited_elements(std::istream & is, character::int_type) -> string;

  auto get_token(std::istream &) -> external_representation;

  auto ignore_nested_block_comment(std::istream &) -> std::istream &;

  auto read_character_literal(std::istream &) -> value_type;

  auto read_string_literal(std::istream &) -> value_type;

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
      assert(standard_input.is_also<std::istream>());
      return static_cast<bool>(standard_input.as<std::istream>());
    }

    inline auto read(std::istream & is) -> value_type
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
          return read_string_literal(is.putback(c));

        case '#':  // 0x23
          switch (auto const c = is.get())
          {
          case '!': // SRFI 22
            is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            return read(is);

          case ',': // SRFI 10
            return evaluate(read(is));

          case ';': // SRFI 62
            return read(is), read(is);

          case '"':
            return string_to_symbol(get_delimited_elements(is.putback(c), c));

          case 'b': // (string->number (read) 2)
            return string_to_number(is.peek() == '#' ? lexical_cast<external_representation>(read(is)) : get_token(is), 2);

          case 'c': // Common Lisp
            {
              let const xs = read(is);
              return make<complex>(list_tail(xs, 0).is<pair>() ? list_ref(xs, 0) : e0,
                                   list_tail(xs, 1).is<pair>() ? list_ref(xs, 1) : e0);
            }

          case 'd':
            return string_to_number(is.peek() == '#' ? lexical_cast<external_representation>(read(is)) : get_token(is), 10);

          case 'e':
            return apply<exact>(read(is)); // NOTE: Same as #,(exact (read))

          case 'f':
            get_token(is);
            return f;

          case 'i':
            return apply<inexact>(read(is)); // NOTE: Same as #,(inexact (read))

          case 'o':
            return string_to_number(is.peek() == '#' ? lexical_cast<external_representation>(read(is)) : get_token(is), 8);

          case 't':
            get_token(is);
            return t;

          case 'x':
            return string_to_number(is.peek() == '#' ? lexical_cast<external_representation>(read(is)) : get_token(is), 16);

          case '(':
            is.putback(c);
            return make<vector>(read(is));

          case '\\':
            return read_character_literal(is);

          case '|': // SRFI 30
            ignore_nested_block_comment(is);
            return read(is);

          default:
            throw read_error(make<string>("unknown discriminator"), make<character>(c));
          }

        case '\'': // 0x27
          return list(string_to_symbol("quote"), read(is));

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
          return string_to_symbol(get_delimited_elements(is.putback(c), c));

        case '(':
        case '[':
        case '{':
          try
          {
            let const kar = read(is);
            return cons(kar, read(is.putback(c)));
          }
          catch (std::integral_constant<char_type, ')'> const&) { return character::eq(c, '(') ? unit : throw; }
          catch (std::integral_constant<char_type, ']'> const&) { return character::eq(c, '[') ? unit : throw; }
          catch (std::integral_constant<char_type, '}'> const&) { return character::eq(c, '{') ? unit : throw; }
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

        default:
          if (auto const token = get_token(is.putback(c)); token == ".")
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

    inline auto read(const_reference x) -> decltype(auto)
    {
      assert(x.is_also<std::istream>());
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

    static auto string_to_number(external_representation const& token, int radix = 10)
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

    static auto string_to_symbol(external_representation const& name) -> const_reference
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
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_READER_HPP
