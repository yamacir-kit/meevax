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

#include <meevax/iostream/ignore.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  string::string(std::istream & is, std::size_t k)
  {
    for (auto c = character(is); size() < k and not std::char_traits<char>::eq(std::char_traits<char>::eof(), c.codepoint); c = character(is))
    {
      switch (c.codepoint)
      {
      case '"':
        return;

      case '\\':
        switch (auto const c = character(is); c.codepoint)
        {
        case 'a': emplace_back('\a'); break;
        case 'b': emplace_back('\b'); break;
        case 'f': emplace_back('\f'); break;
        case 'n': emplace_back('\n'); break;
        case 'r': emplace_back('\r'); break;
        case 't': emplace_back('\t'); break;
        case 'v': emplace_back('\v'); break;

        case 'x':
          if (std::string token; std::getline(is, token, ';') and is.ignore(1))
          {
            if (std::stringstream ss; ss << std::hex << token)
            {
              if (character::value_type value = 0; ss >> value)
              {
                emplace_back(value);
                break;
              }
            }
          }
          throw read_error(make<string>("invalid escape sequence"));

        case '\n':
        case '\r':
          ignore(is, [](auto c) { return std::isspace(c); });
          break;

        default:
          push_back(c);
          break;
        }
        break;

      default:
        push_back(c);
        break;
      }
    }

    throw read_error(make<string>("unterminated string"), unit);
  }

  string::string(std::istream && is)
    : string { is }
  {}

  string::string(std::string const& s)
    : string { std::stringstream(s + "\"") }
  {}

  auto string::list(size_type from, size_type to) const -> object
  {
    let x = unit;

    for (auto iter = std::prev(rend(), to); iter != std::prev(rend(), from); ++iter)
    {
      x = cons(make(*iter), x);
    }

    return x;
  }

  auto string::list(size_type from) const -> object
  {
    return list(from, size());
  }

  string::operator std::string() const
  {
    std::string result;

    for (character const& each : *this)
    {
      result.append(static_cast<std::string>(each));
    }

    return result;
  }

  auto operator <<(std::ostream & os, string const& datum) -> std::ostream &
  {
    auto write = [&](character const& c) -> decltype(auto)
    {
      if (c.codepoint < 0x80)
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

    for (auto const& each : datum)
    {
      write(each);
    }

    return os << cyan("\"");
  }
} // namespace kernel
} // namespace meevax
