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

#ifndef INCLUDED_MEEVAX_BITSET_SIMPLE_BITSET_HPP
#define INCLUDED_MEEVAX_BITSET_SIMPLE_BITSET_HPP

#include <array>

namespace meevax
{
inline namespace bitset
{
  template <auto N>
  struct simple_bitset
  {
    std::array<bool, N> data {};

    auto test(std::size_t i) const noexcept -> bool
    {
      return data[i];
    }

    auto set(std::size_t i) noexcept -> void
    {
      data[i] = true;
    }

    auto reset(std::size_t i) noexcept -> void
    {
      data[i] = false;
    }
  };
} // namespace bitset
} // namespace meevax

#endif // INCLUDED_MEEVAX_BITSET_SIMPLE_BITSET_HPP
