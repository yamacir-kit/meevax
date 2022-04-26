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

#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_UNDERLYING_CAST_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_UNDERLYING_CAST_HPP

#include <meevax/type_traits/requires.hpp>

namespace meevax
{
inline namespace type_traits
{
  template <typename T, REQUIRES(std::is_enum<T>)>
  constexpr auto underlying_cast(T x)
  {
    return static_cast<typename std::underlying_type<T>::type>(x);
  }

  template <typename T>
  constexpr auto underlying_decrement(T && value)
  {
    return typename std::decay<T>::type(underlying_cast(value) - 1);
  }
} // namespace type_traits
} // namespace meevax

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_UNDERLYING_CAST_HPP
