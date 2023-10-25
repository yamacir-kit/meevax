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

#ifndef INCLUDED_MEEVAX_KERNEL_BASIS_HPP
#define INCLUDED_MEEVAX_KERNEL_BASIS_HPP

#include <array>

namespace meevax
{
inline namespace kernel
{
  template <typename... Ts>
  constexpr auto make_array(Ts&&... xs) -> std::array<std::decay_t<std::common_type_t<Ts...>>, sizeof...(Ts)>
  {
    return { std::forward<decltype(xs)>(xs)... };
  }

  constexpr auto basis()
  {
    return make_array(
      R"##(${meevax.ss})##",
      R"##(${r4rs.ss})##",
      R"##(${r5rs.ss})##",
      R"##(${r7rs.ss})##",
      R"##(${srfi-0.ss})##",
      R"##(${srfi-1.ss})##",
      R"##(${srfi-4.ss})##",
      R"##(${srfi-6.ss})##",
      R"##(${srfi-8.ss})##",
      R"##(${srfi-9.ss})##",
      R"##(${srfi-11.ss})##",
      R"##(${srfi-16.ss})##",
      R"##(${srfi-23.ss})##",
      R"##(${srfi-31.ss})##",
      R"##(${srfi-34.ss})##",
      R"##(${srfi-38.ss})##",
      R"##(${srfi-39.ss})##",
      R"##(${srfi-45.ss})##",
      R"##(${srfi-78.ss})##",
      R"##(${srfi-98.ss})##",
      R"##(${srfi-111.ss})##",
      R"##(${srfi-149.ss})##");
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_BASIS_HPP
