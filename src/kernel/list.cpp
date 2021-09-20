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
  auto take(let const& exp, std::size_t size) -> let
  {
    if (0 < size)
    {
      return car(exp) | take(cdr(exp), --size);
    }
    else
    {
      return unit;
    }
  }

  auto append(let const& x, let const& y) -> let
  {
    if (x.is<null>())
    {
      return y;
    }
    else
    {
      return cons(car(x), append(cdr(x), y));
    }
  }

  auto reverse(let const& x) -> let
  {
    return x ? append(reverse(cdr(x)), list(car(x))) : unit;
  }

  auto zip(let const& x, let const& y) -> let
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

  auto unzip2(pair::const_reference xs) -> std::tuple<pair::value_type, pair::value_type>
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
