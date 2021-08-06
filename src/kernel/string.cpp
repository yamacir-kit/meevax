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

#include <iomanip>

#include <meevax/iostream/ignore.hpp>
#include <meevax/kernel/error.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/parser.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  string::string(std::istream & is)
    : characters { read(is) }
  {}

  string::string(std::string const& s)
  {
    std::stringstream ss;
    ss << s << "\""; // XXX HACK
    static_cast<characters &>(*this) = read(ss);
  }

  string::string(string::size_type size, character const& c)
    : characters { size, c }
  {}

  string::operator codeunits() const // NOTE: codeunits = std::string
  {
    codeunits result;

    for (auto const& each : *this)
    {
      result.push_back(each); // NOTE: Character's implicit codepoint->codeunit conversion.
    }

    return result;
  }

  auto string::read(std::istream & is) const -> characters
  {
    auto hex_scalar = [](std::istream & is)
    {
      if (std::string token; std::getline(is, token, ';') and is.ignore(1))
      {
        if (std::stringstream ss; ss << std::hex << token)
        {
          if (codepoint value = 0; ss >> value)
          {
            return value;
          }
        }
      }

      throw tagged_read_error<character>(
        make<string>("invalid escape sequence"), unit);
    };

    characters cs;

    for (auto c = character(is); not is_eof(c.codepoint); c = character(is))
    {
      switch (c.codepoint)
      {
      case '"':
        return cs;

      case '\\':
        switch (auto const c = character(is); c.codepoint)
        {
        case 'a': cs.emplace_back('\a'); break;
        case 'b': cs.emplace_back('\b'); break;
        case 'f': cs.emplace_back('\f'); break;
        case 'n': cs.emplace_back('\n'); break;
        case 'r': cs.emplace_back('\r'); break;
        case 't': cs.emplace_back('\t'); break;
        case 'v': cs.emplace_back('\v'); break;

        case 'x':
          cs.emplace_back(hex_scalar(is));
          break;

        case '\n':
        case '\r':
          ignore(is, is_intraline_whitespace);
          break;

        default:
          cs.push_back(c);
          break;
        }
        break;

      default:
        cs.push_back(c);
        break;
      }
    }

    throw tagged_read_error<string>(
      make<string>("unterminated string"), unit);
  }

  auto string::write_string(std::ostream & os) const -> std::ostream &
  {
    return os << static_cast<codeunits const&>(*this);
  }

  auto operator <<(std::ostream & os, string const& datum) -> std::ostream &
  {
    auto print = [&](character const& c) -> decltype(auto)
    {
      if (c.codepoint < 0x80)
      {
        switch (c.codepoint)
        {
        case '\a': return os << red << "\\a";
        case '\b': return os << red << "\\b";
        case '\t': return os << red << "\\t";
        case '\n': return os << red << "\\n";
        case '\r': return os << red << "\\r";
        case '\"': return os << red << "\\\"";
        case '\\': return os << red << "\\\\";
        case '|':  return os << red << "\\|";

        default:
          return os << cyan << static_cast<char>(c.codepoint);
        }
      }
      else
      {
        return os << red << "\\x" << std::hex << std::uppercase << c.codepoint << ";";
      }
    };

    os << cyan << "\"";

    for (auto const& each : datum)
    {
      print(each);
    }

    return os << cyan << "\"" << reset;
  }
} // namespace kernel
} // namespace meevax
