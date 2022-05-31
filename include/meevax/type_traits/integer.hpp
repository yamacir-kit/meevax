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

#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_INTEGER_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_INTEGER_HPP

#include <cstdint>
#include <type_traits>

namespace meevax
{
inline namespace memory
{
  template <auto Byte>
  using intN_t = typename std::conditional_t<Byte == 1, std::int8_t,
                 typename std::conditional_t<Byte == 2, std::int16_t,
                 typename std::conditional_t<Byte == 4, std::int32_t,
                 typename std::conditional_t<Byte == 8, std::int64_t, void>>>>;

  static_assert(std::is_same<intN_t<1>, std::int8_t>::value);
  static_assert(std::is_same<intN_t<2>, std::int16_t>::value);
  static_assert(std::is_same<intN_t<4>, std::int32_t>::value);
  static_assert(std::is_same<intN_t<8>, std::int64_t>::value);

  static_assert(std::is_same<intN_t<sizeof(float)>, std::int32_t>::value);
  static_assert(std::is_same<intN_t<sizeof(double)>, std::int64_t>::value);

  template <auto Byte>
  using uintN_t = typename std::conditional_t<Byte == 1, std::uint8_t,
                  typename std::conditional_t<Byte == 2, std::uint16_t,
                  typename std::conditional_t<Byte == 4, std::uint32_t,
                  typename std::conditional_t<Byte == 8, std::uint64_t, void>>>>;
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_INTEGER_HPP
