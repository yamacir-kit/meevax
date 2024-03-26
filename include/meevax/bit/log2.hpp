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

#ifndef INCLUDED_MEEVAX_BIT_LOG2_HPP
#define INCLUDED_MEEVAX_BIT_LOG2_HPP

namespace meevax
{
inline namespace bit
{
  template <typename T>
  constexpr auto log2(T x) noexcept -> T
  {
    return (x < 2) ? 1 : log2(x / 2) + 1;
  }

  static_assert(log2(0b0001) == 1);
  static_assert(log2(0b0010) == 2);
  static_assert(log2(0b0011) == 2);
  static_assert(log2(0b0100) == 3);
  static_assert(log2(0b0101) == 3);
  static_assert(log2(0b0110) == 3);
  static_assert(log2(0b0111) == 3);
  static_assert(log2(0b1000) == 4);
} // namespace bit
} // namespace meevax

#endif // INCLUDED_MEEVAX_BIT_LOG2_HPP
