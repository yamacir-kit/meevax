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

#ifndef INCLUDED_MEEVAX_UTILITY_POINTER_TO_HPP
#define INCLUDED_MEEVAX_UTILITY_POINTER_TO_HPP

#include <type_traits>

namespace meevax
{
inline namespace utility
{
  template <typename... Ts>
  using pointer_to = typename std::add_pointer<Ts...>::type;

  template <typename... Ts>
  using const_pointer_to = typename std::add_const<pointer_to<Ts...>>::type;

  static_assert(std::is_same<void      *      ,       pointer_to<      void>>::value);
  static_assert(std::is_same<void      * const, const pointer_to<      void>>::value);
  static_assert(std::is_same<void      * const, const_pointer_to<      void>>::value);
  static_assert(std::is_same<void const*      ,       pointer_to<const void>>::value);
  static_assert(std::is_same<void const* const, const pointer_to<const void>>::value);
  static_assert(std::is_same<void const* const, const_pointer_to<const void>>::value);

  using void_pointer = pointer_to<void>;

  using const_void_pointer = typename std::add_const<void_pointer>::type;
} // namespace utility
} // namespace meevax

#endif // INCLUDED_MEEVAX_UTILITY_POINTER_TO_HPP
