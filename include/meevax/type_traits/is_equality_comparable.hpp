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

#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_IS_EQUALITY_COMPARABLE_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_IS_EQUALITY_COMPARABLE_HPP

#include <type_traits>

namespace meevax
{
inline namespace type_traits
{
  template <typename T, typename = void>
  struct is_equality_comparable
    : public std::false_type
  {};

  template <typename T>
  struct is_equality_comparable<T, std::void_t<decltype(std::declval<T>() == std::declval<T>())>>
    : public std::true_type
  {};

  template <typename T, typename U, typename = void>
  struct is_equality_comparable_with
    : public std::false_type
  {};

  template <typename T, typename U>
  struct is_equality_comparable_with<T, U, std::void_t<decltype(std::declval<T>() == std::declval<U>())>>
    : public std::true_type
  {};
} // namespace type_traits
} // namespace meevax

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_IS_EQUALITY_COMPARABLE_HPP
