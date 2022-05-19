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
  auto take(const_reference x, std::size_t size) -> object
  {
    return 0 < size ? car(x) | take(cdr(x), --size) : unit;
  }

  auto append2(const_reference x, const_reference y) -> object
  {
    return x.is<null>() ? y : cons(car(x), append2(cdr(x), y));
  }

  auto reverse(const_reference x) -> object
  {
    return x ? append2(reverse(cdr(x)), list(car(x))) : unit;
  }

  auto zip(const_reference x, const_reference y) -> object
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

  auto unzip1(const_reference xs) -> object
  {
    return map(car, xs);
  }

  auto unzip2(const_reference xs) -> std::tuple<object, object>
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
