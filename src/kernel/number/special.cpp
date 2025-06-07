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

#include <meevax/kernel/number/special.hpp>

namespace meevax::inline kernel::number
{
  #define DEFINE_UNPROVIDED_REAL2(CMATH)                                       \
  auto CMATH(object const&, object const&) -> object                           \
  {                                                                            \
    throw error(make<string>("The mathematical special function std::" #CMATH " is not provided in this environment.")); \
  }

  #if __cpp_lib_math_special_functions
  DEFINE_REAL2(cyl_bessel_j)
  DEFINE_REAL2(cyl_neumann)
  #else
  DEFINE_UNPROVIDED_REAL2(cyl_bessel_j)
  DEFINE_UNPROVIDED_REAL2(cyl_neumann)
  #endif
} // namespace meevax::kernel::number
