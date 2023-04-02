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
#include <meevax/kernel/string.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator +(symbol const& a, symbol const& b) -> std::string
  {
    return a.std_string + b.std_string;
  }

  auto operator ==(symbol const& a, symbol const& b) -> bool { return a.std_string == b.std_string; }
  auto operator !=(symbol const& a, symbol const& b) -> bool { return a.std_string != b.std_string; }
  auto operator < (symbol const& a, symbol const& b) -> bool { return a.std_string <  b.std_string; }
  auto operator <=(symbol const& a, symbol const& b) -> bool { return a.std_string <= b.std_string; }
  auto operator > (symbol const& a, symbol const& b) -> bool { return a.std_string >  b.std_string; }
  auto operator >=(symbol const& a, symbol const& b) -> bool { return a.std_string >= b.std_string; }

  auto operator <<(std::ostream & os, symbol const& datum) -> std::ostream &
  {
    if (datum.std_string.empty())
    {
      return os << "||";
    }
    else if (auto iter = std::find_if(std::begin(datum.std_string), std::end(datum.std_string), [](auto c)
             {
               return std::iscntrl(c) or std::isspace(c);
             });
             iter != std::end(datum.std_string))
    {
      return os << cyan("#") << string(datum.std_string);
    }
    else
    {
      return os << datum.std_string;
    }
  }

  std::unordered_map<std::string, object> symbols;

  auto string_to_symbol(std::string const& name) -> object const&
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
      throw error(make<string>("failed to intern a symbol"),
                  make<string>(name));
    }
  }
} // namespace kernel
} // namespace meevax
