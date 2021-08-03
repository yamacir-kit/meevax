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

namespace meevax
{
inline namespace kernel
{
  std::unordered_map<std::string, let> const constants
  {
    // R7RS 7.1.1. Lexical structure
    { "+inf.0", make<system_float>(+system_float::infinity()) },
    { "-inf.0", make<system_float>(-system_float::infinity()) },

    { "+nan.0", make<system_float>(+system_float::quiet_NaN()) },
    { "-nan.0", make<system_float>(-system_float::quiet_NaN()) },

    // SRFI-144
    { "fl-pi", make<system_float>(boost::math::constants::pi<system_float::value_type>()) },
  };
} // namespace kernel
} // namespace meevax
