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

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/input_string_port.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  string::string(std::string const& s)
  {
    for (auto input = input_string_port(s);
         not character::is_eof(static_cast<std::istream &>(input).peek());
         vector.emplace_back(input.take_codepoint()));
  }

  string::string(std::size_t const size, character const& c)
    : vector { size, c }
  {}

  string::operator std::string() const
  {
    std::string result;

    for (character const& each : vector)
    {
      result.append(static_cast<std::string>(each));
    }

    return result;
  }

  auto operator ==(string const& s1, string const& s2) -> bool
  {
    return std::equal(std::begin(s1.vector), std::end(s1.vector),
                      std::begin(s2.vector), std::end(s2.vector));
  }

  auto operator <<(std::ostream & os, string const& datum) -> std::ostream &
  {
    auto put = [&](character const& c) -> decltype(auto)
    {
      if (std::isprint(c.codepoint))
      {
        switch (c.codepoint)
        {
        case '\a': return os << red("\\a");
        case '\b': return os << red("\\b");
        case '\t': return os << red("\\t");
        case '\n': return os << red("\\n");
        case '\r': return os << red("\\r");
        case '\"': return os << red("\\\"");
        case '\\': return os << red("\\\\");
        case '|':  return os << red("\\|");

        default:
          return os << cyan(static_cast<char>(c.codepoint));
        }
      }
      else
      {
        return os << red("\\x", std::hex, std::uppercase, c.codepoint, ";");
      }
    };

    os << cyan("\"");

    for (auto const& each : datum.vector)
    {
      put(each);
    }

    return os << cyan("\"");
  }
} // namespace kernel
} // namespace meevax
