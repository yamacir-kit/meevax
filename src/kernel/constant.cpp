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

#include <meevax/kernel/constant.hpp>
#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  std::unordered_map<external_representation, value_type> const constants
  {
    // R7RS 7.1.1. Lexical structure
    { "+inf.0", make(+std::numeric_limits<double>::infinity()) },
    { "-inf.0", make(-std::numeric_limits<double>::infinity()) },

    { "+nan.0", make(+std::numeric_limits<double>::quiet_NaN()) },
    { "-nan.0", make(-std::numeric_limits<double>::quiet_NaN()) },

    // SRFI-144
    { "fl-e",         make(M_E       ) },
    { "fl-log2-e",    make(M_LOG2E   ) },
    { "fl-log10-e",   make(M_LOG10E  ) },
    { "fl-log-2",     make(M_LN2     ) },
    { "fl-1/log-2",   make(M_LN2     ) },
    { "fl-log-10",    make(M_LN10    ) },
    { "fl-1/log-10",  make(M_LN10    ) },
    { "fl-pi",        make(M_PI      ) },
    { "fl-1/pi",      make(M_1_PI    ) },
    { "fl-pi/2",      make(M_PI_2    ) },
    { "fl-pi/4",      make(M_PI_4    ) },
    { "fl-2/pi",      make(M_2_PI    ) },
    { "fl-2/sqrt-pi", make(M_2_SQRTPI) },
    { "fl-sqrt-2",    make(M_SQRT2   ) },
    { "fl-1/sqrt-2",  make(M_SQRT1_2 ) },
  };
} // namespace kernel
} // namespace meevax
