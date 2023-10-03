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

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  auto make_list(std::size_t size, object const& x) -> object
  {
    return 0 < size ? cons(x, make_list(--size, x)) : unit;
  }

  auto is_list(object const& x0, object const& y0) -> bool
  {
    if (x0.is<pair>())
    {
      if (let const& x1 = cdr(x0); x1.is<pair>())
      {
        let const& x2 = cdr(x1),
                   y1 = cdr(y0);

        return not eq(x2, y1) and is_list(x2, y1);
      }
      else
      {
        return x1.is<null>();
      }
    }
    else
    {
      return x0.is<null>();
    }
  }

  auto is_list(object const& xs) -> bool
  {
    return is_list(xs, xs);
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
    return std::distance(xs.begin(), xs.end());
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

  auto memq(object const& x, object const& xs) -> object const&
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

  auto memv(object const& x, object const& xs) -> object const&
  {
    if (xs.is<pair>())
    {
      if (eqv(x, car(xs)))
      {
        return xs;
      }
      else
      {
        return memv(x, cdr(xs));
      }
    }
    else
    {
      return f;
    }
  }

  auto assq(object const& x, object const& xs) -> object const&
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

  auto assv(object const& x, object const& xs) -> object const&
  {
    if (xs.is<pair>())
    {
      if (eqv(x, caar(xs)))
      {
        return car(xs);
      }
      else
      {
        return assv(x, cdr(xs));
      }
    }
    else
    {
      return f;
    }
  }

  auto longest_common_tail(let const& a, let const& b) -> object const&
  {
    if (a.is<null>() or b.is<null>() or eq(a, b))
    {
      return a;
    }
    else
    {
      let const& x = longest_common_tail(a, cdr(b));
      let const& y = longest_common_tail(cdr(a), b);
      return length(x) < length(y) ? y : x;
    }
  }
} // namespace kernel
} // namespace meevax
