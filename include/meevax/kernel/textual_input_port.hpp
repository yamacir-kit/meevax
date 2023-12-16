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

#ifndef INCLUDED_MEEVAX_KERNEL_TEXTUAL_INPUT_PORT_HPP
#define INCLUDED_MEEVAX_KERNEL_TEXTUAL_INPUT_PORT_HPP

#include <istream>

#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/input_port.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/textual_port.hpp>

namespace meevax
{
inline namespace kernel
{
  struct textual_input_port : public virtual textual_port, public virtual input_port
  {
    struct iterator
    {
      using iterator_category = std::forward_iterator_tag;

      using value_type = object;

      using reference = std::add_lvalue_reference_t<value_type>;

      using const_reference = std::add_const_t<reference>;

      using pointer = std::add_pointer_t<value_type>;

      using difference_type = std::ptrdiff_t;

      using size_type = std::size_t;

      textual_input_port * input = nullptr;

      value_type value = eof_object;

      iterator() = default;

      explicit iterator(textual_input_port &);

      auto operator *() -> reference;

      auto operator ->() -> pointer;

      auto operator ++() -> iterator &;

      auto operator ++(int) -> iterator;
    };

    std::unordered_map<std::string, object> datum_labels;

    bool case_sensitive = true;

    auto begin() -> iterator;

    auto end() -> iterator;

    auto get() -> object; // character or eof

    auto get(std::size_t) -> object; // string or eof

    auto get_line() -> object; // string or eof

    auto get_ready() const -> bool;

    auto good() const -> bool;

    auto ignore(std::size_t) -> textual_input_port &;

    auto peek() -> object;

    auto peek_codepoint() -> character::int_type;

    auto read() -> object;

    auto read_character_literal() -> character;

    auto read_string_literal() -> string;

    auto take_codepoint() -> character::int_type;

    auto take_digits() -> std::string;

    auto take_nested_block_comment() -> void; // TODO return std::string

    auto take_token() -> std::string;

    virtual auto istream() -> std::istream & = 0;

    virtual auto istream() const -> std::istream const& = 0;
  };

  auto operator ==(textual_input_port::iterator const&,
                   textual_input_port::iterator const&) -> bool;

  auto operator !=(textual_input_port::iterator const&,
                   textual_input_port::iterator const&) -> bool;

  constexpr auto is_special_character(character::int_type c)
  {
    auto one_of = [c](auto... xs) constexpr
    {
      return (character::eq(c, xs) or ...);
    };

    /*
       A special character is a character that is treated specially by
       textual_input_port::read.
    */
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
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_TEXTUAL_INPUT_PORT_HPP
