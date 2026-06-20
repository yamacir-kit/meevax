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
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SMALL_INTEGER_HPP
