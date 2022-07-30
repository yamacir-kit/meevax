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

#ifndef INCLUDED_MEEVAX_KERNEL_TYPE_INDEX_HPP
#define INCLUDED_MEEVAX_KERNEL_TYPE_INDEX_HPP

#include <array>
#include <functional>
#include <typeindex>

namespace meevax
{
inline namespace kernel
{
  template <auto N>
  struct type_index
  {
    std::array<std::type_info const*, N> type_infos;

    template <typename... Ts>
    explicit type_index(Ts&&... xs)
      : type_infos { std::addressof(xs)... }
    {}

    auto hash_code() const
    {
      return std::accumulate(std::begin(type_infos), std::end(type_infos), 0, [](auto&& lhs, auto&& rhs)
             {
               return lhs xor rhs->hash_code(); // BAD HASHING
             });
    }
  };

  template <auto N>
  auto operator ==(type_index<N> const& lhs, type_index<N> const& rhs)
  {
    for (auto i = 0; i < N; ++i)
    {
      if (lhs.type_infos[i] != rhs.type_infos[i])
      {
        return false;
      }
    }

    return true;
  }
} // namespace kernel
} // namespace meevax

namespace std
{
  template <auto N>
  struct hash<meevax::type_index<N>>
  {
    auto operator()(meevax::type_index<N> const& index) const
    {
      return index.hash_code();
    }
  };
}

#endif // INCLUDED_MEEVAX_KERNEL_TYPE_INDEX_HPP
