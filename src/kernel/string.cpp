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
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/vector.hpp>

namespace meevax
{
inline namespace kernel
{
  string::string(std::string const& s)
  {
    for (auto port = std::stringstream(s); not character::is_eof(port.peek()); codepoints.emplace_back(get_codepoint(port)));
  }

  string::string(const_reference v, const_reference begin, const_reference end)
  {
    std::for_each(std::next(std::begin(v.as<vector>().data), begin.as<exact_integer>()),
                  std::next(std::begin(v.as<vector>().data), end.as<exact_integer>()),
                  [&](let const& c)
                  {
                    codepoints.push_back(c.as<character>());
                  });
  }

  string::string(const_reference xs)
  {
    for (let const& x : xs)
    {
      codepoints.push_back(x.as<character>());
    }
  }

  string::string(const_reference k, const_reference c)
    : codepoints { k.as<exact_integer>(), c.as<character>() }
  {}

  auto string::copy(const_reference at, const_reference from, const_reference begin, const_reference end) -> void
  {
    codepoints.reserve(codepoints.size() + from.as<string>().codepoints.size());

    std::copy(std::next(std::begin(from.as<string>().codepoints), begin.as<exact_integer>()),
              std::next(std::begin(from.as<string>().codepoints), end.as<exact_integer>()),
              std::next(std::begin(codepoints), at.as<exact_integer>()));
  }

  auto string::length() const -> value_type
  {
    return make<exact_integer>(codepoints.size());
  }

  auto string::make_list(const_reference from, const_reference to) const -> value_type
  {
    return std::accumulate(std::prev(std::rend(codepoints), to.as<exact_integer>()),
                           std::prev(std::rend(codepoints), from.as<exact_integer>()),
                           unit,
                           [](let const& xs, character const& c)
                           {
                             return cons(make(c), xs);
                           });
  }

  auto string::ref(const_reference k) const -> value_type
  {
    return make(codepoints.at(k.as<exact_integer>()));
  }

  auto string::set(const_reference k, const_reference c) -> void
  {
    codepoints.at(k.as<exact_integer>()) = c.as<character>();
  }

  string::operator std::string() const
  {
    std::string result;

    for (character const& each : codepoints)
    {
      result.append(static_cast<std::string>(each));
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

    for (auto const& each : datum.codepoints)
    {
      write(each);
    }

    return os << cyan("\"");
  }
} // namespace kernel
} // namespace meevax
