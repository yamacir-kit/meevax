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

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  auto equal(object const& x, object const& y) -> bool
  {
    if (x.is<null>() and y.is<null>())
    {
      return true;
    }
    else if (x.is<pair>() and y.is<pair>())
    {
      return equal(car(x), car(y)) and equal(cdr(x), cdr(y));
    }
    else
    {
      return eqv(x, y);
    }
  }
} // namespace kernel
} // namespace meevax
