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
#include <meevax/kernel/writer.hpp>

namespace meevax
{
inline namespace kernel
{
  auto write_simple(std::ostream & os, pair const& datum) -> std::ostream &
  {
    os << magenta("(");

    write_simple(os, car(datum));

    for (auto iter = std::begin(cdr(datum)); iter != unit; ++iter)
    {
      if (iter.get().is<pair>())
      {
        os << " ";

        write_simple(os, *iter);
      }
      else // iter is the last element of dotted-list.
      {
        os << magenta(" . ");

        write_simple(os, iter.get());

        return os << magenta(")");
      }
    }

    return os << magenta(")");
  }

  auto write_simple(std::ostream & os, const_reference x) -> std::ostream &
  {
    return x.is<pair>() ? write_simple(os, x.as<pair>()) : os << x;
  }
} // namespace kernel
} // namespace meevax
