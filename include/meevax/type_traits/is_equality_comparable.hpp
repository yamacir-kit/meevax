/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_IS_EQUALITY_COMPARABLE_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_IS_EQUALITY_COMPARABLE_HPP

#include <type_traits>

namespace meevax::inline type_traits
{
  template <typename T, typename U, typename = void>
  struct is_equality_comparable
    : public std::false_type
  {};

  template <typename T>
  struct is_equality_comparable<T, T, std::void_t<decltype(std::declval<T>() == std::declval<T>())>>
    : public std::true_type
  {};

  template <typename T, typename U>
  struct is_equality_comparable<T, U, std::void_t<decltype(std::declval<T>() == std::declval<U>())>>
    : public std::true_type
  {};

  template <typename T, typename U = T>
  inline constexpr bool is_equality_comparable_v = is_equality_comparable<T, U>::value;
} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_IS_EQUALITY_COMPARABLE_HPP
