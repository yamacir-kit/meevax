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

#ifndef INCLUDED_MEEVAX_FUNCTIONAL_ARITHMETIC_OPERATION_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_ARITHMETIC_OPERATION_HPP

#include <functional>
#include <numeric>
#include <ostream>

namespace meevax
{
inline namespace functional
{
  #define DEFINE(TYPENAME, BASE, ...)                                          \
  struct TYPENAME : public std::BASE<void> {} constexpr __VA_ARGS__;           \
  auto operator <<(std::ostream &, TYPENAME const&) -> std::ostream &

  DEFINE(addition,       plus      , add);
  DEFINE(division,       divides   , div);
  DEFINE(modulo,         modulus   , mod);
  DEFINE(multiplication, multiplies, mul);
  DEFINE(subtraction,    minus     , sub);

  #undef DEFINE

  #define DEFINE(TYPENAME, FUNCTION, ...)                                      \
  struct TYPENAME                                                              \
  {                                                                            \
    template <typename... Ts>                                                  \
    constexpr auto operator ()(Ts&&... xs) const                               \
      -> decltype(std::FUNCTION(std::forward<decltype(xs)>(xs)...))            \
    {                                                                          \
      return FUNCTION(std::forward<decltype(xs)>(xs)...);                      \
    }                                                                          \
  } constexpr __VA_ARGS__

  DEFINE(greatest_common_divisor, gcd, gcd);
  DEFINE(least_common_multiple  , lcm, lcm);

  #undef DEFINE
} // namespace functional
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_ARITHMETIC_OPERATION_HPP
