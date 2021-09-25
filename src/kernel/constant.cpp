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

#include <meevax/kernel/constant.hpp>
#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  std::unordered_map<std::string, pair::value_type> const constants
  {
    // R7RS 7.1.1. Lexical structure
    { "+inf.0", make<f64>(+f64::infinity()) },
    { "-inf.0", make<f64>(-f64::infinity()) },

    { "+nan.0", make<f64>(+f64::quiet_NaN()) },
    { "-nan.0", make<f64>(-f64::quiet_NaN()) },

    // SRFI-144
    { "fl-e",         make<f64>(M_E       ) },
    { "fl-log2-e",    make<f64>(M_LOG2E   ) },
    { "fl-log10-e",   make<f64>(M_LOG10E  ) },
    { "fl-log-2",     make<f64>(M_LN2     ) },
    { "fl-1/log-2",   make<f64>(M_LN2     ) },
    { "fl-log-10",    make<f64>(M_LN10    ) },
    { "fl-1/log-10",  make<f64>(M_LN10    ) },
    { "fl-pi",        make<f64>(M_PI      ) },
    { "fl-1/pi",      make<f64>(M_1_PI    ) },
    { "fl-pi/2",      make<f64>(M_PI_2    ) },
    { "fl-pi/4",      make<f64>(M_PI_4    ) },
    { "fl-2/pi",      make<f64>(M_2_PI    ) },
    { "fl-2/sqrt-pi", make<f64>(M_2_SQRTPI) },
    { "fl-sqrt-2",    make<f64>(M_SQRT2   ) },
    { "fl-1/sqrt-2",  make<f64>(M_SQRT1_2 ) },
  };
} // namespace kernel
} // namespace meevax
