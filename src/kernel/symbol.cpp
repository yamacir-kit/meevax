/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
#include <meevax/kernel/textual_input_port.hpp>

namespace meevax::inline kernel
{
  auto operator +(symbol const& a, symbol const& b) -> std::string
  {
    return a.name + b.name;
  }

  auto operator <<(std::ostream & os, symbol const& datum) -> std::ostream &
  {
    if (datum.name.empty())
    {
      return os << "||";
    }
    else if (std::find_if(datum.name.begin(), datum.name.end(), is_special_character) != datum.name.end())
    {
      return os << cyan("#") << string(datum.name);
    }
    else
    {
      return os << datum.name;
    }
  }

  auto symbols() -> std::unordered_map<std::string, object> &
  {
    static auto symbols = std::unordered_map<std::string, object>();
    return symbols;
  }

  auto make_symbol(std::string const& name) -> object const&
  {
    if (auto const iter = symbols().find(name); iter != symbols().end())
    {
      return iter->second;
    }
    else if (auto const [iter, success] = symbols().emplace(name, make<symbol>(name)); success)
    {
      return iter->second;
    }
    else
    {
      throw error(make<string>("failed to intern a symbol"),
                  make<string>(name));
    }
  }
} // namespace meevax::kernel
