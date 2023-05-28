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

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  auto make_list(std::size_t size, object const& x) -> object
  {
    return 0 < size ? cons(x, make_list(--size, x)) : unit;
  }

  auto last(object const& xs) -> object const&
  {
    return cdr(xs).is<pair>() ? last(cdr(xs)) : car(xs);
  }

  auto take(object const& x, std::size_t size) -> object
  {
    return 0 < size ? cons(car(x), take(cdr(x), --size)) : unit;
  }

  auto length(object const& xs) -> std::size_t
  {
    return std::distance(std::begin(xs), std::end(xs));
  }

  auto append(object const& x, object const& y) -> object
  {
    if (x.is<pair>())
    {
      return cons(car(x), append(cdr(x), y));
    }
    else
    {
      return y;
    }
  }

  auto reverse(object const& xs, object const& a) -> object
  {
    if (xs.is<pair>())
    {
      return reverse(cdr(xs), cons(car(xs), a));
    }
    else
    {
      return a;
    }
  }

  auto memq(object const& x, object const& xs) -> object
  {
    if (xs.is<pair>())
    {
      if (eq(x, car(xs)))
      {
        return xs;
      }
      else
      {
        return memq(x, cdr(xs));
      }
    }
    else
    {
      return f;
    }
  }

  auto assq(object const& x, object const& xs) -> object
  {
    if (xs.is<pair>())
    {
      if (eq(x, caar(xs)))
      {
        return car(xs);
      }
      else
      {
        return assq(x, cdr(xs));
      }
    }
    else
    {
      return f;
    }
  }
} // namespace kernel
} // namespace meevax
