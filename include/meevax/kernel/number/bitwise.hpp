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

#ifndef INCLUDED_MEEVAX_KERNEL_NUMBER_BITWISE_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMBER_BITWISE_HPP

#include <meevax/kernel/number.hpp>

namespace meevax::inline kernel::number
{
  auto bitwise_not(object const& x) -> object;

  auto bitwise_and(object const& x, object const& y) -> object;
  auto bitwise_ior(object const& x, object const& y) -> object;
  auto bitwise_xor(object const& x, object const& y) -> object;

  auto bit_shift(object const&, small_integer) -> object;
  auto bit_count(object const&) -> object;
  auto bit_width(object const&) -> object;
} // namespace meevax::kernel::number

#endif // INCLUDED_MEEVAX_KERNEL_NUMBER_BITWISE_HPP
