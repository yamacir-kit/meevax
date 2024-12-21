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

#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_IS_ARRAY_SUBSCRIPTABLE_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_IS_ARRAY_SUBSCRIPTABLE_HPP

#include <type_traits>

namespace meevax::inline type_traits
{
  template <typename T, typename = void>
  [[deprecated]]
  struct is_array_subscriptable : public std::false_type
  {};

  template <typename T>
  [[deprecated]]
  struct is_array_subscriptable<T, std::void_t<decltype(std::declval<T>()[std::declval<std::size_t>()])>>
    : public std::true_type
  {};

  template <typename T>
  [[deprecated]]
  inline constexpr auto is_array_subscriptable_v = is_array_subscriptable<T>::value;
} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_IS_ARRAY_SUBSCRIPTABLE_HPP
