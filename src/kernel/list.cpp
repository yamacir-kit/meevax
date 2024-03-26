/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
    if (0 < size)
    {
      return cons(x, make_list(--size, x));
    }
    else
    {
      return nullptr;
    }
  }

  auto iota(std::size_t count, object const& start, object const& step) -> object
  {
    if (0 < count)
    {
      return cons(start, iota(count - 1, start + step, step));
    }
    else
    {
      return nullptr;
    }
  }

  auto is_list(object const& x0, object const& y0) -> bool
  {
    if (x0.is<pair>())
    {
      if (let const& x1 = cdr(x0); x1.is<pair>())
      {
        let const& x2 = cdr(x1);
        let const& y1 = cdr(y0);

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

  auto is_circular_list(object const& x0, object const& y0) -> bool
  {
    if (x0.is<pair>())
    {
      if (let const& x1 = cdr(x0); x1.is<pair>())
      {
        let const& x2 = cdr(x1);
        let const& y1 = cdr(y0);

        return eq(x2, y1) or is_circular_list(x2, y1);
      }
      else
      {
        return false;
      }
    }
    else
    {
      return false;
    }
  }

  auto is_circular_list(object const& xs) -> bool
  {
    return is_circular_list(xs, xs);
  }

  auto is_dotted_list(object const& x0, object const& y0) -> bool
  {
    if (x0.is<pair>())
    {
      if (let const& x1 = cdr(x0); x1.is<pair>())
      {
        let const& x2 = cdr(x1);
        let const& y1 = cdr(y0);

        return not eq(x2, y1) and is_dotted_list(x2, y1);
      }
      else
      {
        return not x1.is<null>();
      }
    }
    else
    {
      return not x0.is<null>();
    }
  }

  auto is_dotted_list(object const& xs) -> bool
  {
    return is_dotted_list(xs, xs);
  }

  auto list_copy(object const& xs) -> object
  {
    if (xs.is<pair>())
    {
      return cons(car(xs), list_copy(cdr(xs)));
    }
    else
    {
      return xs;
    }
  }

  auto take(object const& x, std::size_t k) -> object
  {
    if (0 < k)
    {
      return cons(car(x), take(cdr(x), k - 1));
    }
    else
    {
      return nullptr;
    }
  }

  auto take(object & x, std::size_t k) -> object
  {
    if (0 < k)
    {
      cdr(drop(x, k - 1)) = nullptr;
      return x;
    }
    else
    {
      return nullptr;
    }
  }

  auto take_right(object const& x, object const& y) -> object const&
  {
    if (y.is<pair>())
    {
      return take_right(cdr(x), cdr(y));
    }
    else
    {
      return x;
    }
  }

  auto take_right(object const& x, std::size_t k) -> object const&
  {
    return take_right(x, drop(x, k));
  }

  auto drop(object const& x, std::size_t k) -> object const&
  {
    if (0 < k)
    {
      return drop(cdr(x), k - 1);
    }
    else
    {
      return x;
    }
  }

  auto drop(object & x, std::size_t k) -> object &
  {
    if (0 < k)
    {
      return drop(cdr(x), k - 1);
    }
    else
    {
      return x;
    }
  }

  auto drop_right(object const& x, object const& y) -> object
  {
    if (y.is<pair>())
    {
      return cons(car(x), drop_right(cdr(x), cdr(y)));
    }
    else
    {
      return nullptr;
    }
  }

  auto drop_right(object const& x, std::size_t k) -> object
  {
    return drop_right(x, drop(x, k));
  }

  auto drop_right(object & x, object const& y) -> void
  {
    if (y.is<pair>())
    {
      drop_right(cdr(x), cdr(y));
    }
    else
    {
      cdr(x) = nullptr;
    }
  }

  auto drop_right(object & x, std::size_t k) -> object
  {
    if (let const& y = drop(x, k); y.is<pair>())
    {
      drop_right(x, cdr(y));
      return x;
    }
    else
    {
      return nullptr;
    }
  }

  auto length(object const& x) -> std::ptrdiff_t
  {
    return std::distance(x.begin(), x.end());
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

  auto append(object & x, object const& y) -> object &
  {
    if (x.is<null>())
    {
      return x = y;
    }
    else if (y.is<null>())
    {
      return x;
    }
    else
    {
      cdr(last_pair(x)) = y;
      return x;
    }
  }

  auto append_reverse(object const& x, object const& y) -> object
  {
    if (x.is<null>())
    {
      return y;
    }
    else
    {
      return append_reverse(cdr(x), cons(car(x), y));
    }
  }

  auto append_reverse(object & x, object const& y) -> object
  {
    if (x.is<null>())
    {
      return y;
    }
    else
    {
      return append_reverse(std::exchange(cdr(x), y), x);
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

  auto reverse(object const& xs) -> object
  {
    return reverse(xs, nullptr);
  }

  auto reverse(object & xs, object const& a) -> object
  {
    if (xs.is<null>())
    {
      return a;
    }
    else
    {
      return reverse(std::exchange(cdr(xs), a), xs);
    }
  }

  auto reverse(object & xs) -> object
  {
    return reverse(xs, nullptr);
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

  auto alist_cons(object const& key, object const& datum, object const& alist) -> object
  {
    return cons(cons(key, datum), alist);
  }

  auto alist_copy(object const& alist) -> object
  {
    return map([](auto&& x)
               {
                 return cons(car(x), cdr(x));
               },
               alist);
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
