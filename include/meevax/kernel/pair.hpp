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
    std::uint16_t size = sizeof(pair);

    pair()
      : std::pair<object, object> { nullptr, nullptr }
    {}

    template <typename... Ts>
    requires std::constructible_from<std::pair<object, object>, Ts...>
    explicit pair(Ts&&... xs)
      : std::pair<object, object> { std::forward<decltype(xs)>(xs)... }
    {}

    virtual ~pair() = default;

    auto virtual eqv(pair const*) const -> bool;

    auto extent() const noexcept -> std::pair<void const*, void const*>;

    auto virtual type() const noexcept -> std::type_info const&;

    auto virtual write(std::ostream &) const -> std::ostream &;
  };

  static_assert(sizeof(pair) == CHAR_BIT * 4);

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

  auto inline car = [](auto&& x) -> decltype(auto) { return std::get<0>(x.template as<pair>()); };
  auto inline cdr = [](auto&& x) -> decltype(auto) { return std::get<1>(x.template as<pair>()); };

  auto inline caar = [](auto&& x) -> decltype(auto) { return car(car(std::forward<decltype(x)>(x))); };
  auto inline cadr = [](auto&& x) -> decltype(auto) { return car(cdr(std::forward<decltype(x)>(x))); };
  auto inline cdar = [](auto&& x) -> decltype(auto) { return cdr(car(std::forward<decltype(x)>(x))); };
  auto inline cddr = [](auto&& x) -> decltype(auto) { return cdr(cdr(std::forward<decltype(x)>(x))); };

  auto inline caaar = [](auto&& x) -> decltype(auto) { return car(caar(std::forward<decltype(x)>(x))); };
  auto inline caadr = [](auto&& x) -> decltype(auto) { return car(cadr(std::forward<decltype(x)>(x))); };
  auto inline cadar = [](auto&& x) -> decltype(auto) { return car(cdar(std::forward<decltype(x)>(x))); };
  auto inline caddr = [](auto&& x) -> decltype(auto) { return car(cddr(std::forward<decltype(x)>(x))); };
  auto inline cdaar = [](auto&& x) -> decltype(auto) { return cdr(caar(std::forward<decltype(x)>(x))); };
  auto inline cdadr = [](auto&& x) -> decltype(auto) { return cdr(cadr(std::forward<decltype(x)>(x))); };
  auto inline cddar = [](auto&& x) -> decltype(auto) { return cdr(cdar(std::forward<decltype(x)>(x))); };
  auto inline cdddr = [](auto&& x) -> decltype(auto) { return cdr(cddr(std::forward<decltype(x)>(x))); };

  auto inline caaaar = [](auto&& x) -> decltype(auto) { return car(caaar(std::forward<decltype(x)>(x))); };
  auto inline caaadr = [](auto&& x) -> decltype(auto) { return car(caadr(std::forward<decltype(x)>(x))); };
  auto inline caadar = [](auto&& x) -> decltype(auto) { return car(cadar(std::forward<decltype(x)>(x))); };
  auto inline caaddr = [](auto&& x) -> decltype(auto) { return car(caddr(std::forward<decltype(x)>(x))); };
  auto inline cadaar = [](auto&& x) -> decltype(auto) { return car(cdaar(std::forward<decltype(x)>(x))); };
  auto inline cadadr = [](auto&& x) -> decltype(auto) { return car(cdadr(std::forward<decltype(x)>(x))); };
  auto inline caddar = [](auto&& x) -> decltype(auto) { return car(cddar(std::forward<decltype(x)>(x))); };
  auto inline cadddr = [](auto&& x) -> decltype(auto) { return car(cdddr(std::forward<decltype(x)>(x))); };
  auto inline cdaaar = [](auto&& x) -> decltype(auto) { return cdr(caaar(std::forward<decltype(x)>(x))); };
  auto inline cdaadr = [](auto&& x) -> decltype(auto) { return cdr(caadr(std::forward<decltype(x)>(x))); };
  auto inline cdadar = [](auto&& x) -> decltype(auto) { return cdr(cadar(std::forward<decltype(x)>(x))); };
  auto inline cdaddr = [](auto&& x) -> decltype(auto) { return cdr(caddr(std::forward<decltype(x)>(x))); };
  auto inline cddaar = [](auto&& x) -> decltype(auto) { return cdr(cdaar(std::forward<decltype(x)>(x))); };
  auto inline cddadr = [](auto&& x) -> decltype(auto) { return cdr(cdadr(std::forward<decltype(x)>(x))); };
  auto inline cdddar = [](auto&& x) -> decltype(auto) { return cdr(cddar(std::forward<decltype(x)>(x))); };
  auto inline cddddr = [](auto&& x) -> decltype(auto) { return cdr(cdddr(std::forward<decltype(x)>(x))); };

  auto inline caddddr = [](auto&& x) -> decltype(auto) { return car(cddddr(std::forward<decltype(x)>(x))); };
  auto inline cdddddr = [](auto&& x) -> decltype(auto) { return cdr(cddddr(std::forward<decltype(x)>(x))); };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
