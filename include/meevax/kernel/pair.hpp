/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/object.hpp>

namespace meevax::inline kernel
{
  struct pair : public std::pair<object, object>
  {
    pair()
      : std::pair<object, object> { nullptr, nullptr }
    {}

    template <typename T,
              typename U = std::nullptr_t,
              typename = std::enable_if_t<std::is_constructible_v<std::pair<object, object>, T, U>>>
    explicit pair(T&& x, U&& y)
      : std::pair<object, object> { std::forward<decltype(x)>(x), std::forward<decltype(y)>(y) }
    {}

    virtual ~pair() = default;

    auto virtual eqv(pair const*) const -> bool;

    auto virtual extent() const noexcept -> std::pair<void const*, std::size_t>;

    auto virtual contains(void const*) const noexcept -> bool;

    auto virtual type() const noexcept -> std::type_info const&;

    auto virtual write(std::ostream &) const -> std::ostream &;
  };

  auto operator <<(std::ostream &, pair const&) -> std::ostream &;

  auto inline cons = [](auto&&... xs) constexpr -> decltype(auto)
  {
    auto cons = [](auto cons, auto&& x, auto&& y, auto&&... xs) constexpr -> decltype(auto)
    {
      if constexpr (0 < sizeof...(xs))
      {
        return make<pair>(std::forward<decltype(x)>(x),
                          cons(cons,
                               std::forward<decltype(y)>(y),
                               std::forward<decltype(xs)>(xs)...));
      }
      else
      {
        return make<pair>(std::forward<decltype(x)>(x),
                          std::forward<decltype(y)>(y));
      }
    };

    return cons(cons, std::forward<decltype(xs)>(xs)...);
  };

  auto inline xcons = [](auto&& x, auto&& y) constexpr
  {
    return cons(std::forward<decltype(y)>(y),
                std::forward<decltype(x)>(x));
  };

  template <auto N>
  auto get(auto&& x) -> decltype(auto)
  {
    if constexpr (std::is_same_v<std::decay_t<decltype(x)>, object>)
    {
      return std::get<N>(x.template as<pair>());
    }
    else
    {
      return std::get<N>(x);
    }
  }

  inline auto car = [](auto&& x) -> decltype(auto) { return get<0>(std::forward<decltype(x)>(x)); };
  inline auto cdr = [](auto&& x) -> decltype(auto) { return get<1>(std::forward<decltype(x)>(x)); };

  inline auto caar = [](auto&& x) -> decltype(auto) { return car(car(std::forward<decltype(x)>(x))); };
  inline auto cadr = [](auto&& x) -> decltype(auto) { return car(cdr(std::forward<decltype(x)>(x))); };
  inline auto cdar = [](auto&& x) -> decltype(auto) { return cdr(car(std::forward<decltype(x)>(x))); };
  inline auto cddr = [](auto&& x) -> decltype(auto) { return cdr(cdr(std::forward<decltype(x)>(x))); };

  inline auto caaar = [](auto&& x) -> decltype(auto) { return car(caar(std::forward<decltype(x)>(x))); };
  inline auto caadr = [](auto&& x) -> decltype(auto) { return car(cadr(std::forward<decltype(x)>(x))); };
  inline auto cadar = [](auto&& x) -> decltype(auto) { return car(cdar(std::forward<decltype(x)>(x))); };
  inline auto caddr = [](auto&& x) -> decltype(auto) { return car(cddr(std::forward<decltype(x)>(x))); };
  inline auto cdaar = [](auto&& x) -> decltype(auto) { return cdr(caar(std::forward<decltype(x)>(x))); };
  inline auto cdadr = [](auto&& x) -> decltype(auto) { return cdr(cadr(std::forward<decltype(x)>(x))); };
  inline auto cddar = [](auto&& x) -> decltype(auto) { return cdr(cdar(std::forward<decltype(x)>(x))); };
  inline auto cdddr = [](auto&& x) -> decltype(auto) { return cdr(cddr(std::forward<decltype(x)>(x))); };

  inline auto caaaar = [](auto&& x) -> decltype(auto) { return car(caaar(std::forward<decltype(x)>(x))); };
  inline auto caaadr = [](auto&& x) -> decltype(auto) { return car(caadr(std::forward<decltype(x)>(x))); };
  inline auto caadar = [](auto&& x) -> decltype(auto) { return car(cadar(std::forward<decltype(x)>(x))); };
  inline auto caaddr = [](auto&& x) -> decltype(auto) { return car(caddr(std::forward<decltype(x)>(x))); };
  inline auto cadaar = [](auto&& x) -> decltype(auto) { return car(cdaar(std::forward<decltype(x)>(x))); };
  inline auto cadadr = [](auto&& x) -> decltype(auto) { return car(cdadr(std::forward<decltype(x)>(x))); };
  inline auto caddar = [](auto&& x) -> decltype(auto) { return car(cddar(std::forward<decltype(x)>(x))); };
  inline auto cadddr = [](auto&& x) -> decltype(auto) { return car(cdddr(std::forward<decltype(x)>(x))); };
  inline auto cdaaar = [](auto&& x) -> decltype(auto) { return cdr(caaar(std::forward<decltype(x)>(x))); };
  inline auto cdaadr = [](auto&& x) -> decltype(auto) { return cdr(caadr(std::forward<decltype(x)>(x))); };
  inline auto cdadar = [](auto&& x) -> decltype(auto) { return cdr(cadar(std::forward<decltype(x)>(x))); };
  inline auto cdaddr = [](auto&& x) -> decltype(auto) { return cdr(caddr(std::forward<decltype(x)>(x))); };
  inline auto cddaar = [](auto&& x) -> decltype(auto) { return cdr(cdaar(std::forward<decltype(x)>(x))); };
  inline auto cddadr = [](auto&& x) -> decltype(auto) { return cdr(cdadr(std::forward<decltype(x)>(x))); };
  inline auto cdddar = [](auto&& x) -> decltype(auto) { return cdr(cddar(std::forward<decltype(x)>(x))); };
  inline auto cddddr = [](auto&& x) -> decltype(auto) { return cdr(cdddr(std::forward<decltype(x)>(x))); };

  inline auto caddddr = [](auto&& x) -> decltype(auto) { return car(cddddr(std::forward<decltype(x)>(x))); };
  inline auto cdddddr = [](auto&& x) -> decltype(auto) { return cdr(cddddr(std::forward<decltype(x)>(x))); };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
