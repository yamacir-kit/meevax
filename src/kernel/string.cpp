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

#include <iterator>
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
    for (auto c = character(is); std::size(codepoints) < k and not std::char_traits<char>::eq(std::char_traits<char>::eof(), c.codepoint); c = character(is))
    {
      switch (c.codepoint)
      {
      case '"':
        return;

      case '\\':
        switch (auto const c = character(is); c.codepoint)
        {
        case 'a': codepoints.emplace_back('\a'); break;
        case 'b': codepoints.emplace_back('\b'); break;
        case 'f': codepoints.emplace_back('\f'); break;
        case 'n': codepoints.emplace_back('\n'); break;
        case 'r': codepoints.emplace_back('\r'); break;
        case 't': codepoints.emplace_back('\t'); break;
        case 'v': codepoints.emplace_back('\v'); break;

        case 'x':
          if (external_representation token; std::getline(is, token, ';') and is.ignore(1))
          {
            if (std::stringstream ss; ss << std::hex << token)
            {
              if (character::value_type value = 0; ss >> value)
              {
                codepoints.emplace_back(value);
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
          codepoints.push_back(c);
          break;
        }
        break;

      default:
        codepoints.push_back(c);
        break;
      }
    }

    throw read_error(make<string>("unterminated string"), unit);
  }

  string::string(std::istream && is)
    : string { is }
  {}

  string::string(external_representation const& s)
    : string { std::stringstream(s + "\"") }
  {}

  string::string(const_reference xs)
  {
    for (let const& x : xs)
    {
      std::copy(std::begin(x.as<string>().codepoints),
                std::end(x.as<string>().codepoints),
                std::back_inserter(codepoints));
    }
  }

  auto string::copy(const_reference from, const_reference to) const -> value_type
  {
    let const& s = make<string>();

    std::copy(std::next(std::begin(codepoints), from.as<exact_integer>()),
              std::next(std::begin(codepoints), to.as<exact_integer>()),
              std::back_inserter(s.as<string>().codepoints));

    return s;
  }

  auto string::length() const -> value_type
  {
    return make<exact_integer>(codepoints.size());
  }

  auto string::list(std::size_t from, std::size_t to) const -> meevax::value_type
  {
    let x = unit;

    for (auto iter = std::prev(codepoints.rend(), to); iter != std::prev(codepoints.rend(), from); ++iter)
    {
      x = cons(make(*iter), x);
    }

    return x;
  }

  auto string::list(std::size_t from) const -> meevax::value_type
  {
    return list(from, std::size(codepoints));
  }

  auto string::ref(const_reference k) const -> value_type
  {
    return make(codepoints.at(k.as<exact_integer>()));
  }

  auto string::set(const_reference k, const_reference c) -> void
  {
    codepoints.at(k.as<exact_integer>()) = c.as<character>();
  }

  string::operator external_representation() const
  {
    external_representation result;

    for (character const& each : codepoints)
    {
      result.append(static_cast<external_representation>(each));
    }

    return result;
  }

  auto operator ==(string const& s1, string const& s2) -> bool
  {
    return std::equal(std::begin(s1.codepoints), std::end(s1.codepoints),
                      std::begin(s2.codepoints), std::end(s2.codepoints));
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

    for (auto const& each : datum.codepoints)
    {
      write(each);
    }

    return os << cyan("\"");
  }
} // namespace kernel
} // namespace meevax
