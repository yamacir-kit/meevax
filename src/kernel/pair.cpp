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

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  let const unit { nullptr };

  auto operator <<(std::ostream & os, pair const& pare) -> std::ostream &
  {
    os << magenta << "(" << reset << car(pare);

    for (let rest = cdr(pare); rest; rest = cdr(rest))
    {
      if (rest.is<pair>())
      {
        os << " " << car(rest);
      }
      else // iter is the last element of dotted-list.
      {
        os << magenta << " . " << reset << rest;
        break;
      }
    }

    return os << magenta << ")" << reset;
  }
} // namespace kernel
} // namespace meevax
