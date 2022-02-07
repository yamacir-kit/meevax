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

#ifndef INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <typeinfo>

namespace meevax
{
inline namespace memory
{
  /* ---- Tagged Pointers --------------------------------------------------- */

  using word = std::uintptr_t;

  /* ---- Linux 64 Bit Address Space -------------------------------------------
   *
   * user   0x0000 0000 0000 0000 ~ 0x0000 7FFF FFFF FFFF
   * kernel 0xFFFF 8000 0000 0000 ~
   *
   * ------------------------------------------------------------------------ */

  static_assert(8 <= sizeof(word));

  template <typename T, typename = void>
  struct is_immediate
    : public std::false_type
  {};

  template <typename T>
  struct is_immediate<T,
    typename std::enable_if<(sizeof(T) <= sizeof(word) / 2)>::type>
    : public std::true_type
  {};


  /* ---- Tag ------------------------------------------------------------------
   *
   * ┌─────┬──────────────────────────────────────────────────────────────────┐
   * │ Tag │ Purpose                                                          │
   * ├─────┼──────────────────────────────────────────────────────────────────┤
   * │ 000 │ T* or std::nullptr_t                                             │
   * │ 001 │                                                                  │
   * │ 010 │                                                                  │
   * │ 011 │                                                                  │
   * │ 100 │                                                                  │
   * │ 101 │                                                                  │
   * │ 110 │                                                                  │
   * │ 111 │                                                                  │
   * └─────┴──────────────────────────────────────────────────────────────────┘
   *
   * ------------------------------------------------------------------------ */

  constexpr std::uintptr_t mask { 0x07 };

  template <typename T>
  constexpr auto tag_of(T const* const address)
  {
    return reinterpret_cast<std::uintptr_t>(address) & mask;
  }

  template <typename T>
  constexpr auto type_of(T const* const address) -> const std::type_info&
  {
    switch (tag_of(address))
    {
    case 0:
      return typeid(decltype(address));

    default:
      return typeid(void);
    }
  }

  // template <typename T>
  // constexpr auto unbox(T const* const data) -> std::uintptr_t
  // {
  //   switch (tag_of(data))
  //   {
  //   case tag<std::int32_t>::value:
  //     return unbox(data);
  //
  //   default:
  //     throw std::logic_error { "unexpected immediate value" };
  //   }
  // }
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_TAGGED_POINTER_HPP
