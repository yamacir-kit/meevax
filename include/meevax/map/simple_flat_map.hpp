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

#ifndef INCLUDED_MEEVAX_MAP_SIMPLE_FLAT_MAP_HPP
#define INCLUDED_MEEVAX_MAP_SIMPLE_FLAT_MAP_HPP

#include <utility>
#include <vector>

namespace meevax
{
inline namespace map
{
  template <typename Key, typename Value>
  struct simple_flat_map : public std::vector<std::pair<Key, Value>>
  {
    auto lower_bound(Key const& key) -> decltype(auto)
    {
      auto compare = [](auto && pair, auto && key) constexpr
      {
        return pair.first < key;
      };

      return std::lower_bound(this->begin(), this->end(), key, compare);
    }

    template <typename... Ts>
    auto emplace_hint(Ts&&... xs) -> decltype(auto)
    {
      return this->emplace(std::forward<decltype(xs)>(xs)...);
    }
  };
} // namespace map
} // namespace meevax

#endif // INCLUDED_MEEVAX_MAP_SIMPLE_FLAT_MAP_HPP
