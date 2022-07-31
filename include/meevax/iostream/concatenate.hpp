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

#ifndef INCLUDED_MEEVAX_IOSTREAM_CONCATENATE_HPP
#define INCLUDED_MEEVAX_IOSTREAM_CONCATENATE_HPP

#include <sstream>

namespace meevax
{
inline namespace iostream
{
  inline auto concatenate = [](auto&&... xs)
  {
    std::stringstream ss;
    (ss << ... << xs);
    return ss.str();
  };
} // namespace iostream
} // namespace meevax

#endif // INCLUDED_MEEVAX_IOSTREAM_CONCATENATE_HPP