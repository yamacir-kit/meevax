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
#include <meevax/kernel/reader.hpp> // get_codepoint
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/textual_input_port.hpp>

namespace meevax
{
inline namespace kernel
{
  auto textual_input_port::get() -> object
  {
    try
    {
      return make<character>(get_codepoint(*this));
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
        s.codepoints.emplace_back(get_codepoint(*this));
      }

      return make(s);
    }
    catch (eof const&)
    {
      return eof_object;
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
    try
    {
      auto g = static_cast<std::istream &>(*this).tellg();
      let c = make<character>(get_codepoint(*this));
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
      return interaction_environment().as<environment>().read(*this);
    }
    catch (eof const&)
    {
      return eof_object;
    }
  }
} // namespace kernel
} // namespace meevax
