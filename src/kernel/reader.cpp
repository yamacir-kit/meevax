/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax
{
inline namespace kernel
{
  auto circulate(object const& xs, object const& x, std::string const& n) -> void
  {
    if (xs.is<pair>())
    {
      circulate(car(xs), x, n);

      if (cdr(xs).is<datum_label>() and cdr(xs).as<datum_label>().n == n)
      {
        cdr(xs) = x;
      }
      else
      {
        circulate(cdr(xs), x, n);
      }
    }
  }

  auto circulate(object const& xs, std::string const& n) -> void
  {
    return circulate(xs, xs, n);
  }
} // namespace kernel
} // namespace meevax
