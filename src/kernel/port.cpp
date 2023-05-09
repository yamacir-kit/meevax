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
#include <meevax/kernel/interaction_environment.hpp>
#include <meevax/kernel/port.hpp>
#include <meevax/kernel/reader.hpp> // get_codepoint

namespace meevax
{
inline namespace kernel
{
  auto port::is_open() const -> bool
  {
    return true;
  }

  auto port::close() -> void
  {}

  auto textual_input_port::get() -> object
  {
    try
    {
      return make<character>(get_codepoint(static_cast<std::istream &>(*this)));
    }
    catch (eof const&)
    {
      return eof_object;
    }
  }

  auto textual_input_port::get(std::size_t size) -> object
  {
    try
    {
      auto s = string();

      for (std::size_t i = 0; i < size; ++i)
      {
        s.codepoints.emplace_back(get_codepoint(static_cast<std::istream &>(*this)));
      }

      return make(s);
    }
    catch (eof const&)
    {
      return eof_object;
    }
  }

  auto textual_input_port::peek() -> object
  {
    try
    {
      auto g = static_cast<std::istream &>(*this).tellg();
      let c = make<character>(get_codepoint(static_cast<std::istream &>(*this)));
      static_cast<std::istream &>(*this).seekg(g);
      return c;
    }
    catch (eof const&)
    {
      return eof_object;
    }
  }

  auto textual_input_port::read() -> object
  {
    try
    {
      return interaction_environment().as<environment>().read(static_cast<std::istream &>(*this));
    }
    catch (eof const&)
    {
      return eof_object;
    }
  }

  auto textual_input_port::get_ready() -> bool
  {
    return static_cast<bool>(static_cast<std::istream &>(*this));
  }

  auto textual_output_port::flush() -> void
  {
    static_cast<std::ostream &>(*this) << std::flush;
  }

  auto textual_output_port::put(character const& c) -> void
  {
    static_cast<std::ostream &>(*this) << static_cast<std::string>(c);
  }

  auto textual_output_port::put(string const& s) -> void
  {
    static_cast<std::ostream &>(*this) << static_cast<std::string>(s);
  }

  auto textual_output_port::write(object const& x) -> void
  {
    static_cast<std::ostream &>(*this) << x;
  }

  auto textual_output_port::write_simple(object const& x) -> void
  {
    meevax::write_simple(static_cast<std::ostream &>(*this), x);
  }

  standard_input_port::operator std::istream &()
  {
    return std::cin;
  }

  auto operator <<(std::ostream & output, standard_input_port const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("standard-input-port") << magenta(")");
  }

  standard_output_port::operator std::ostream &()
  {
    return std::cout;
  }

  auto operator <<(std::ostream & output, standard_output_port const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("standard-output-port") << magenta(")");
  }

  standard_error_port::operator std::ostream &()
  {
    return std::cerr;
  }

  auto operator <<(std::ostream & output, standard_error_port const&) -> std::ostream &
  {
    return output << magenta("#,(") << blue("standard-error-port") << magenta(")");
  }
} // namespace kernel
} // namespace meevax
