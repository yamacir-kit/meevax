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

#include <meevax/kernel/eof.hpp>
#include <meevax/kernel/include.hpp>
#include <meevax/kernel/input_file_port.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  auto include(object const& names, bool case_sensitive, object const& xs) -> object
  {
    auto open = [&](object const& name)
    {
      auto port = input_file_port(name.as<string>());
      port.case_sensitive = case_sensitive;
      return port;
    };

    if (names.is<pair>())
    {
      auto include_aux = [&](auto include_aux, auto&& input, object const& xs) -> object
      {
        if (let const& x = input.read(); not x.is<eof>())
        {
          return include_aux(include_aux, input, cons(x, xs));
        }
        else
        {
          return include(cdr(names), case_sensitive, xs);
        }
      };

      return include_aux(include_aux, open(car(names)), xs);
    }
    else
    {
      return reverse(xs);
    }
  }
} // namespace kernel
} // namespace meevax
