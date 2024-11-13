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

#ifndef INCLUDED_MEEVAX_BIT_BIT_CAST_HPP
#define INCLUDED_MEEVAX_BIT_BIT_CAST_HPP

#include <cstring>
#include <type_traits>

namespace meevax
{
inline namespace bit
{
  template <typename To,
            typename From,
            typename = std::enable_if_t<std::conjunction_v<std::bool_constant<sizeof(To) == sizeof(From)>,
                                                           std::is_trivially_copyable<To>,
                                                           std::is_trivially_copyable<From>,
                                                           std::is_trivially_constructible<To>>>>
  [[deprecated]]
  auto bit_cast(From const& from) noexcept
  {
    To to;
    std::memcpy(&to, &from, sizeof from);
    return to;
  }
} // namespace bit
} // namespace meevax

#endif // INCLUDED_MEEVAX_BIT_BIT_CAST_HPP
