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
  auto take(object const& x, std::size_t size) -> object
  {
    return 0 < size ? cons(car(x), take(cdr(x), --size)) : unit;
  }

  auto append2(object const& x, object const& y) -> object
  {
    return x.is<null>() ? y : cons(car(x), append2(cdr(x), y));
  }

  auto reverse(object const& x) -> object
  {
    return x ? append2(reverse(cdr(x)), list(car(x))) : unit;
  }

  auto zip(object const& x, object const& y) -> object
  {
    if (x.is<null>() and y.is<null>())
    {
      return unit;
    }
    else if (x.is<pair>() and y.is<pair>())
    {
      return list(car(x), car(y)) | zip(cdr(x), cdr(y));
    }
    else
    {
      return unit;
    }
  }

  auto unzip1(object const& xs) -> object
  {
    return map1(car, xs);
  }

  auto unzip2(object const& xs) -> std::tuple<object, object>
  {
    if (xs.is<null>())
    {
      return std::make_tuple(unit, unit);
    }
    else
    {
      auto const& x = car(xs);

      auto const& [a, b] = unzip2(cdr(xs));

      return std::make_tuple(cons( car(x), a),
                             cons(cadr(x), b));
    }
  }
} // namespace kernel
} // namespace meevax
