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

#include <meevax/kernel/number/floating_point_manipulation.hpp>

namespace meevax::inline kernel::number
{
  auto ldexp(object const& x, object const& y) -> object
  {
    auto f = [](auto&& x, auto&& y)
    {
      return std::ldexp(static_cast<double>(std::forward<decltype(x)>(x)),
                        static_cast<int   >(std::forward<decltype(y)>(y)));
    };

    return apply_to<real_numbers>(f, x, y);
  }

  DEFINE_REAL2(nextafter)

  DEFINE_REAL2(copysign)
} // namespace meevax::kernel::number
