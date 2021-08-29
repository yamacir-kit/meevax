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

#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <algorithm>
#include <numeric>

#include <meevax/functional/combinator.hpp>
#include <meevax/kernel/equivalence.hpp>
#include <meevax/kernel/exact_integer.hpp>
#include <meevax/kernel/iterator.hpp>

namespace meevax
{
inline namespace kernel
{
  auto to_native_reference = [](auto&& x) -> decltype(auto)
  {
    if constexpr (std::is_same<typename std::decay<decltype(x)>::type, let>::value)
    {
      return x.load();
    }
    else if constexpr (std::is_same<typename std::decay<decltype(x)>::type, homoiconic_iterator>::value)
    {
      return x.unwrap().load();
    }
    else
    {
      return std::forward<decltype(x)>(x);
    }
  };

  auto car = [](auto&& x) noexcept -> decltype(auto)
  {
    return std::get<0>(to_native_reference(std::forward<decltype(x)>(x)));
  };

  auto cdr = [](auto&& x) noexcept -> decltype(auto)
  {
    return std::get<1>(to_native_reference(std::forward<decltype(x)>(x)));
  };

  template <typename T, typename U, REQUIRES(std::is_convertible<T, let>,
                                             std::is_convertible<U, let>)>
  auto operator |(T&& x, U&& y) -> decltype(auto)
  {
    return make<pair>(std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
  }

  auto cons = [](auto&&... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ...);
  };

  auto list = [](auto&& ... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ... | unit);
  };

  auto xcons = [](auto&& d, auto&& a) constexpr
  {
    return cons(std::forward<decltype(a)>(a), std::forward<decltype(d)>(d));
  };

  auto make_list = [](std::size_t k, let const& x = unit)
  {
    let result = list();

    for (std::size_t i = 0; i < k; ++i)
    {
      result = cons(x, result);
    }

    return result;
  };

  auto list_tabulate = [](auto n, auto&& initialize)
  {
    let x = list();

    while (0 <= --n)
    {
      x = cons(initialize(n), x);
    }

    return x;
  };

  auto list_copy = [](auto const& x)
  {
    auto copy = [](auto&& rec, let const& x) -> let
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

  auto circular_list = [](auto&&... xs)
  {
    let x = list(std::forward<decltype(xs)>(xs)...);

    if (auto const length = std::distance(std::cbegin(x), std::cend(x)); 0 < length)
    {
      cdr(std::next(std::begin(x), length - 1)) = x;
    }

    return x;
  };

  constexpr auto caar = compose(car, car);
  constexpr auto cadr = compose(car, cdr);
  constexpr auto cdar = compose(cdr, car);
  constexpr auto cddr = compose(cdr, cdr);

  constexpr auto caaar = compose(car, caar);
  constexpr auto caadr = compose(car, cadr);
  constexpr auto cadar = compose(car, cdar);
  constexpr auto caddr = compose(car, cddr);
  constexpr auto cdaar = compose(cdr, caar);
  constexpr auto cdadr = compose(cdr, cadr);
  constexpr auto cddar = compose(cdr, cdar);
  constexpr auto cdddr = compose(cdr, cddr);

  constexpr auto caaaar = compose(car, caaar);
  constexpr auto caaadr = compose(car, caadr);
  constexpr auto caadar = compose(car, cadar);
  constexpr auto caaddr = compose(car, caddr);
  constexpr auto cadaar = compose(car, cdaar);
  constexpr auto cadadr = compose(car, cdadr);
  constexpr auto caddar = compose(car, cddar);
  constexpr auto cadddr = compose(car, cdddr);
  constexpr auto cdaaar = compose(cdr, caaar);
  constexpr auto cdaadr = compose(cdr, caadr);
  constexpr auto cdadar = compose(cdr, cadar);
  constexpr auto cdaddr = compose(cdr, caddr);
  constexpr auto cddaar = compose(cdr, cdaar);
  constexpr auto cddadr = compose(cdr, cdadr);
  constexpr auto cdddar = compose(cdr, cddar);
  constexpr auto cddddr = compose(cdr, cdddr);

  auto list_tail = [](auto&& x, auto&& k) -> decltype(auto)
  {
    if constexpr (std::is_same<typename std::decay<decltype(k)>::type, let>::value)
    {
      return std::next(std::cbegin(std::forward<decltype(x)>(x)), static_cast<std::size_t>(k.template as<exact_integer>()));
    }
    else
    {
      return std::next(std::cbegin(std::forward<decltype(x)>(x)), std::forward<decltype(k)>(k));
    }
  };

  auto list_ref = [](auto&&... xs) constexpr -> decltype(auto)
  {
    return car(list_tail(std::forward<decltype(xs)>(xs)...));
  };

  let take(let const& exp, std::size_t size);

  auto length = [](auto const& x) constexpr
  {
    return std::distance(std::cbegin(x), std::cend(x));
  };

  let append(let const&, let const&);

  let reverse(let const&);

  let zip(let const&, let const&);

  template <typename F>
  let map(F&& f, let const& x)
  {
    if (x.is<null>())
    {
      return unit;
    }
    else
    {
      return cons(f(car(x)), map(f, cdr(x)));
    }
  }

  auto find = [](let const& x, auto&& predicate) constexpr -> pair::const_reference
  {
    if (auto const& iter = std::find_if(std::cbegin(x), std::cend(x), std::forward<decltype(predicate)>(predicate)); iter)
    {
      return *iter;
    }
    else
    {
      return f;
    }
  };

  auto assoc = [](let const& key, let const& alist, auto&& compare = equivalence_comparator<2>()) -> pair::const_reference
  {
    return find(alist, [&](auto&& each)
    {
      return compare(car(each), key);
    });
  };

  auto assv = [](auto&&... xs) -> pair::const_reference
  {
    return assoc(std::forward<decltype(xs)>(xs)..., equivalence_comparator<1>());
  };

  auto assq = [](auto&&... xs) -> pair::const_reference
  {
    return assoc(std::forward<decltype(xs)>(xs)..., equivalence_comparator<0>());
  };

  auto alist_cons = [](auto&& key, auto&& datum, auto&& alist) constexpr
  {
    return cons(cons(key, datum), alist);
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIST_HPP
