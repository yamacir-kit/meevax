/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_FUNCTIONAL_INEXACT_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_INEXACT_HPP

#include <functional>
#include <cmath>

namespace meevax
{
inline namespace functional
{
  #define DEFINE(FUNCTION)                                     \
  struct FUNCTION                                              \
  {                                                            \
    template <typename... Ts>                                  \
    auto operator ()(Ts&&... xs) const -> decltype(auto)       \
    {                                                          \
      return std::FUNCTION(std::forward<decltype(xs)>(xs)...); \
    }                                                          \
  }

  DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh);
  DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh);
  DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh);
               DEFINE(atan2);

  DEFINE(exp);
  DEFINE(log);

  #undef DEFINE
} // namespace functional
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_INEXACT_HPP
