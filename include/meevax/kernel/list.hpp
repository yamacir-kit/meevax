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
    if constexpr (std::is_same_v<std::decay_t<T>, iterator>)
    {
      return get<N>(x.get());
    }
    else if constexpr (std::is_same_v<std::decay_t<T>, object>)
    {
      return std::get<N>(x.template as<pair>());
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

  template <typename T, typename U, REQUIRES(std::is_convertible<T, object>,
                                             std::is_convertible<U, object>)>
  auto operator |(T&& x, U&& y) -> decltype(auto)
  {
    return make<pair>(std::forward<decltype(x)>(x), std::forward<decltype(y)>(y));
  }

  inline auto cons = [](auto&&... xs) constexpr
  {
    return (xs | ...);
  };

  inline auto list = [](auto&&... xs) constexpr
  {
    return (xs | ... | unit);
  };

  inline auto xcons = [](auto&& d, auto&& a) constexpr
  {
    return cons(std::forward<decltype(a)>(a), std::forward<decltype(d)>(d));
  };

  auto make_list(std::size_t, object const& = unit) -> object;

  template <typename T>
  auto tail(T&& x, std::size_t size) -> decltype(x)
  {
    return 0 < size ? tail(cdr(std::forward<decltype(x)>(x)), --size) : x;
  }

  template <typename... Ts>
  auto head(Ts&&... xs) -> decltype(auto)
  {
    return car(tail(std::forward<decltype(xs)>(xs)...));
  }

  auto last(object const&) -> object const&;

  auto take(object const&, std::size_t) -> object;

  auto length(object const&, std::size_t = 0) -> std::size_t;

  auto append(object const&, object const&) -> object;

  auto reverse(object const&, object const& = unit) -> object;

  template <typename F>
  auto map(F f, object const& xs) -> object
  {
    if (xs.is<pair>())
    {
      return cons(f(car(xs)), map(f, cdr(xs)));
    }
    else
    {
      return unit;
    }
  }

  auto memq(object const&, object const&) -> object;

  auto assq(object const&, object const&) -> object;

  template <typename F>
  auto filter(F f, object const& xs) -> object
  {
    if (xs.is<pair>())
    {
      if (f(car(xs)))
      {
        return cons(car(xs), filter(f, cdr(xs)));
      }
      else
      {
        return filter(f, cdr(xs));
      }
    }
    else
    {
      return unit;
    }
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIST_HPP
