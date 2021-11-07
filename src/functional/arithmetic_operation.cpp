/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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

#include <meevax/functional/arithmetic_operation.hpp>

namespace meevax
{
inline namespace functional
{
  #define DEFINE(TYPENAME, ...)                                                \
  auto operator <<(std::ostream & os, TYPENAME const&) -> std::ostream &       \
  {                                                                            \
    return os << __VA_ARGS__;                                                  \
  }                                                                            \
  static_assert(true)

  DEFINE(addition      , "+");
  DEFINE(division      , "/");
  DEFINE(modulo        , "%");
  DEFINE(multiplication, "*");
  DEFINE(subtraction   , "-");

  DEFINE(greatest_common_divisor, "gcd");
  DEFINE(least_common_multiple  , "lcm");

  #undef DEFINE
} // namespace functional
} // namespace meevax

