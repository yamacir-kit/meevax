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

#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <meevax/kernel/boolean.hpp>
#include <meevax/kernel/comparator.hpp>
#include <meevax/kernel/iterator.hpp>

namespace meevax
{
inline namespace kernel
{
  template <auto N, typename T>
  auto get(T&& x) -> decltype(auto)
  {
    if constexpr (std::is_same_v<std::decay_t<decltype(x)>, iterator>)
    {
      return std::get<N>(*x.get());
    }
    else if constexpr (std::is_same_v<std::decay_t<decltype(x)>, object>)
    {
      return x.template is_also<pair>() ? std::get<N>(*x) : unit;
    }
    else
    {
      return std::get<N>(x);
    }
  }

  inline auto car = [](auto&& x) -> decltype(auto)
  {
    return get<0>(std::forward<decltype(x)>(x));
  };

  inline auto cdr = [](auto&& x) -> decltype(auto)
  {
    return get<1>(std::forward<decltype(x)>(x));
  };

  template <typename T, typename U, REQUIRES(std::is_convertible<T, object>,
                                             std::is_convertible<U, object>)>
  auto operator |(T&& x, U&& y) -> decltype(auto)
  {
    return make<pair>(std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
  }

  inline auto cons = [](auto&&... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ...);
  };

  inline auto list = [](auto&& ... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ... | unit);
  };

  inline auto xcons = [](auto&& d, auto&& a) constexpr
  {
    return cons(std::forward<decltype(a)>(a), std::forward<decltype(d)>(d));
  };

  inline auto make_list = [](std::size_t k, object const& x = unit)
  {
    let result = list();

    for (std::size_t i = 0; i < k; ++i)
    {
      result = cons(x, result);
    }

    return result;
  };

  inline auto list_tabulate = [](auto n, auto&& initialize)
  {
    let x = list();

    while (0 <= --n)
    {
      x = cons(initialize(n), x);
    }

    return x;
  };

  inline auto list_copy = [](auto const& x)
  {
    auto copy = [](auto&& rec, object const& x) -> object
    {
      if (x.is<pair>())
      {
        return cons(car(x), rec(rec, cdr(x)));
      }
      else
      {
        return x;
      }
    };

    return z(copy)(x);
  };

  inline auto circular_list = [](auto&&... xs)
  {
    let x = list(std::forward<decltype(xs)>(xs)...);

    if (auto const length = std::distance(std::cbegin(x), std::cend(x)); 0 < length)
    {
      cdr(std::next(std::begin(x), length - 1)) = x;
    }

    return x;
  };

  inline constexpr auto caar = compose(car, car);
  inline constexpr auto cadr = compose(car, cdr);
  inline constexpr auto cdar = compose(cdr, car);
  inline constexpr auto cddr = compose(cdr, cdr);

  inline constexpr auto caaar = compose(car, caar);
  inline constexpr auto caadr = compose(car, cadr);
  inline constexpr auto cadar = compose(car, cdar);
  inline constexpr auto caddr = compose(car, cddr);
  inline constexpr auto cdaar = compose(cdr, caar);
  inline constexpr auto cdadr = compose(cdr, cadr);
  inline constexpr auto cddar = compose(cdr, cdar);
  inline constexpr auto cdddr = compose(cdr, cddr);

  inline constexpr auto caaaar = compose(car, caaar);
  inline constexpr auto caaadr = compose(car, caadr);
  inline constexpr auto caadar = compose(car, cadar);
  inline constexpr auto caaddr = compose(car, caddr);
  inline constexpr auto cadaar = compose(car, cdaar);
  inline constexpr auto cadadr = compose(car, cdadr);
  inline constexpr auto caddar = compose(car, cddar);
  inline constexpr auto cadddr = compose(car, cdddr);
  inline constexpr auto cdaaar = compose(cdr, caaar);
  inline constexpr auto cdaadr = compose(cdr, caadr);
  inline constexpr auto cdadar = compose(cdr, cadar);
  inline constexpr auto cdaddr = compose(cdr, caddr);
  inline constexpr auto cddaar = compose(cdr, cdaar);
  inline constexpr auto cddadr = compose(cdr, cdadr);
  inline constexpr auto cdddar = compose(cdr, cddar);
  inline constexpr auto cddddr = compose(cdr, cdddr);

  inline auto unpair = [](object const& x) // a.k.a car+cdr (SRFI 1)
  {
    return std::forward_as_tuple(car(x), cdr(x));
  };

  template <typename T, typename K, REQUIRES(std::is_integral<K>)>
  auto list_tail(T&& x, K const k) -> object const&
  {
    return 0 < k ? list_tail(cdr(x), k - 1) : x;
  }

  auto take(object const&, std::size_t) -> object;

  inline auto length = [](auto const& x) constexpr
  {
    return std::distance(std::cbegin(x), std::cend(x));
  };

  auto append2(object const&, object const&) -> object;

  auto reverse(object const&) -> object;

  auto zip(object const&, object const&) -> object;

  auto unzip1(object const& xs) -> object;

  auto unzip2(object const& xs) -> std::tuple<object, object>;

  template <typename F>
  auto map1(F&& f, object const& x) -> object
  {
    return x.is<null>() ? unit : cons(f(car(x)), map1(f, cdr(x)));
  }

  inline auto find = [](object const& xs, auto&& compare) constexpr -> object const&
  {
    if (auto&& iter = std::find_if(std::begin(xs), std::end(xs), compare); iter)
    {
      return *iter;
    }
    else
    {
      return f;
    }
  };

  inline auto assoc = [](object const& x, object const& xs, auto&& compare)
  {
    return find(xs, [&](auto&& each) { return compare(x, car(each)); });
  };

  inline auto assv = [](auto&&... xs)
  {
    return assoc(std::forward<decltype(xs)>(xs)..., eqv);
  };

  inline auto assq = [](auto&&... xs)
  {
    return assoc(std::forward<decltype(xs)>(xs)..., eq);
  };

  inline auto alist_cons = [](auto&& key, auto&& datum, auto&& alist)
  {
    return cons(cons(key, datum), alist);
  };

  inline auto member = [](object const& x, object const& xs, auto&& compare)
  {
    if (auto&& iter = std::find_if(std::begin(xs), std::end(xs), [&](auto&& each) { return compare(x, each); }); iter)
    {
      return iter.get();
    }
    else
    {
      return f;
    }
  };

  inline auto memv = [](auto&&... xs)
  {
    return member(std::forward<decltype(xs)>(xs)..., eqv);
  };

  inline auto memq = [](auto&&... xs)
  {
    return member(std::forward<decltype(xs)>(xs)..., eq);
  };

  inline auto filter = [](auto&& satisfy, object const& xs)
  {
    auto filter = [&](auto&& filter, let const& xs)
    {
      if (xs.is<null>())
      {
        return xs;
      }
      else
      {
        if (let const& head = car(xs),
                       rest = cdr(xs); satisfy(head))
        {
          if (let const& filtered = filter(filter, rest); eq(rest, filtered))
          {
            return xs;
          }
          else
          {
            return cons(head, filtered);
          }
        }
        else
        {
          return filter(filter, rest);
        }
      }
    };

    return z(filter)(xs);
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIST_HPP
