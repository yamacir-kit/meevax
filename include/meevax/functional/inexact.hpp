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

#include <meevax/kernel/floating_point.hpp>

namespace meevax
{
inline namespace functional
{
  template <typename U>
  auto inexact_cast(U&& x) // TODO RENAME THIS
  {
    if constexpr (std::is_floating_point_v<std::decay_t<decltype(x)>>)
    {
      return std::forward<decltype(x)>(x);
    }
    else
    {
      return static_cast<double>(std::forward<decltype(x)>(x));
    }
  }

  #define DEFINE(FUNCTION)                                                     \
  struct FUNCTION                                                              \
  {                                                                            \
    template <typename... Ts>                                                  \
    auto operator ()(Ts&&... xs) const                                         \
    {                                                                          \
      return floating_point(std::FUNCTION(inexact_cast(std::forward<decltype(xs)>(xs))...)); \
    }                                                                          \
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
