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

#include <meevax/kernel/number/trigonometric.hpp>

namespace meevax::inline kernel::number
{
  DEFINE_COMPLEX1(sin)
  DEFINE_COMPLEX1(cos)
  DEFINE_COMPLEX1(tan)

  DEFINE_COMPLEX1(asin)
  DEFINE_COMPLEX1(acos)
  DEFINE_COMPLEX1(atan)

  DEFINE_REAL2(atan2)
} // namespace meevax::kernel::number
