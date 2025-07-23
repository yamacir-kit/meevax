/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <filesystem>
#include <istream>

#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/input_port.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/textual_port.hpp>

namespace meevax::inline kernel
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

    auto at_end_of_file() const -> bool;

    auto begin() -> iterator;

    auto end() -> iterator;

    auto get() -> object; // Returns character or eof (for Scheme procedure read-char)

    auto get(std::size_t) -> object; // Returns string or eof (for Scheme procedure read-string)

    auto get_line() -> object; // Returns string or eof (for Scheme procedure read-line)

    auto get_ready() const -> bool;

    auto peek() -> object;

    auto peek_character() -> character;

    auto read(character = {}) -> object;

    auto take_character() -> character;

    template <typename F>
    auto take_character_until(F satisfy, character c = {})
    {
      auto s = static_cast<std::string>(c);

      while (get_ready() and not satisfy(c = take_character()))
      {
        s += c;
      }

      return s;
    }

    template <typename F>
    auto take_character_while(F satisfy, character c = {})
    {
      auto s = static_cast<std::string>(c);

      while (get_ready() and satisfy(peek_character()))
      {
        s += take_character();
      }

      return s;
    }

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
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_TEXTUAL_INPUT_PORT_HPP
