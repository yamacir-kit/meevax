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

#ifndef INCLUDED_MEEVAX_UTILITY_ENUMERATION_HPP
#define INCLUDED_MEEVAX_UTILITY_ENUMERATION_HPP

#include <meevax/type_traits/is_scoped_enum.hpp>
#include <meevax/type_traits/underlying_cast.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T, REQUIRES(is_scoped_enum<T>)>
  constexpr auto operator |(T const e1, T const e2) noexcept
  {
    return static_cast<T>(underlying_cast(e1) | underlying_cast(e2));
  }

  template <typename T, REQUIRES(is_scoped_enum<T>)>
  constexpr auto operator &(T const e1, T const e2) noexcept
  {
    return static_cast<T>(underlying_cast(e1) & underlying_cast(e2));
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_UTILITY_ENUMERATION_HPP
