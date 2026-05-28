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

#ifndef INCLUDED_MEEVAX_KERNEL_SMALL_INTEGER_HPP
#define INCLUDED_MEEVAX_KERNEL_SMALL_INTEGER_HPP

#include <cstdint>

namespace meevax::inline kernel
{
  using small_integer = std::int32_t; // Fixed sized integer that can be boxed.
  using widen_integer = std::int64_t; // Fixed sized integer that is temporarily widened to prevent possible overflow.

  auto constexpr operator ""_i64(unsigned long long int value) { return static_cast<std:: int64_t>(value); }
  auto constexpr operator ""_u64(unsigned long long int value) { return static_cast<std::uint64_t>(value); }

  template <std::size_t>
  struct int8n;

  template <> struct int8n<1> { using type = std::int8_t; };
  template <> struct int8n<2> { using type = std::int16_t; };
  template <> struct int8n<4> { using type = std::int32_t; };
  template <> struct int8n<8> { using type = std::int64_t; };

  template <auto N>
  using int8n_t = typename int8n<N>::type;

  template <std::size_t>
  struct uint8n;

  template <> struct uint8n<1> { using type = std::uint8_t; };
  template <> struct uint8n<2> { using type = std::uint16_t; };
  template <> struct uint8n<4> { using type = std::uint32_t; };
  template <> struct uint8n<8> { using type = std::uint64_t; };

  template <auto N>
  using uint8n_t = typename uint8n<N>::type;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SMALL_INTEGER_HPP
