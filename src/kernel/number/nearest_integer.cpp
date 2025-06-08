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

#include <meevax/kernel/number/nearest_integer.hpp>

namespace meevax::inline kernel::number
{
  #define DEFINE(NAME, CMATH)                                                  \
  auto NAME(object const& x) -> object                                         \
  {                                                                            \
    auto f = []<typename T>(T const& x)                                        \
    {                                                                          \
      if constexpr (std::is_floating_point_v<T>)                               \
      {                                                                        \
        return CMATH(x);                                                       \
      }                                                                        \
      else if constexpr (std::is_same_v<T, ratio>)                             \
      {                                                                        \
        return large_integer(CMATH(static_cast<double>(x)));                   \
      }                                                                        \
      else if constexpr (std::is_same_v<T, large_integer> or std::is_integral_v<T>) \
      {                                                                        \
        return x;                                                              \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return complex(NAME(x.real()),                                         \
                       NAME(x.imag()));                                        \
      }                                                                        \
    };                                                                         \
                                                                               \
    return apply_to<complex_number>(f, x);                                     \
  }

  DEFINE(floor,    std::floor)
  DEFINE(ceiling,  std::ceil)
  DEFINE(truncate, std::trunc)
  DEFINE(round,    std::round)
} // namespace meevax::kernel::number
