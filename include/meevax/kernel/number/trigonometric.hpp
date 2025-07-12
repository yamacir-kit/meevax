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

#ifndef INCLUDED_MEEVAX_KERNEL_NUMBER_TRIGONOMETRIC_HPP
#define INCLUDED_MEEVAX_KERNEL_NUMBER_TRIGONOMETRIC_HPP

#include <meevax/kernel/number.hpp>

namespace meevax::inline kernel::number
{
  auto sin(object const&) -> object;
  auto cos(object const&) -> object;
  auto tan(object const&) -> object;

  auto asin(object const&) -> object;
  auto acos(object const&) -> object;
  auto atan(object const&) -> object;

  auto atan2(object const&, object const&) -> object;
} // namespace meevax::kernel::number

#endif // INCLUDED_MEEVAX_KERNEL_NUMBER_TRIGONOMETRIC_HPP
