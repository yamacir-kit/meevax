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

#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/character.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/memory/gc_pointer.hpp>

namespace meevax
{
inline namespace kernel
{
  struct pair;

  using object = gc_pointer<pair,
                            bool,
                            std::int32_t, std::uint32_t,
                            float,
                            character,
                            instruction>;

  using let = object;

  let extern unit;

  template <typename T,
            typename Allocator = collector::default_allocator<void>,
            typename... Ts>
  auto make(Ts&&... xs) -> object
  {
    return object::allocate<T, Allocator>(std::forward<decltype(xs)>(xs)...);
  }

  template <typename T,
            typename Allocator = collector::default_allocator<void>>
  auto make(T&& x) -> object
  {
    return object::allocate<std::decay_t<T>, Allocator>(std::forward<decltype(x)>(x));
  }

  template <template <typename...> typename Traits,
            typename Allocator = collector::default_allocator<void>,
            typename... Ts,
            REQUIRES(std::is_constructible<typename Traits<Ts...>::type, Ts...>)>
  auto make(Ts&&... xs) -> decltype(auto)
  {
    return make<typename Traits<Ts...>::type, Allocator>(std::forward<decltype(xs)>(xs)...);
  }

  struct pair : public std::pair<object, object>
  {
    template <auto Const>
    struct forward_iterator
    {
      using iterator_category = std::forward_iterator_tag;

      using value_type = object;

      using reference = std::add_lvalue_reference_t<std::conditional_t<Const, std::add_const_t<value_type>, value_type>>;

      using pointer = std::add_pointer_t<reference>;

      using difference_type = std::ptrdiff_t;

      using size_type = std::size_t;

      using node_type = std::conditional_t<Const, pair const*, pair *>;

      node_type current = nullptr;

      node_type initial = nullptr;

      forward_iterator() = default;

      explicit constexpr forward_iterator(node_type current)
        : current { current }
        , initial { current }
      {}

      constexpr auto operator *() const -> reference
      {
        return current->first;
      }

      constexpr auto operator ->() const -> pointer
      {
        return &current->first;
      }

      auto operator ++() -> decltype(auto)
      {
        if (current = current->second.get(); current == initial or (current and current->type() != typeid(pair)))
        {
          current = nullptr;
        }

        return *this;
      }

      auto operator ++(int) -> decltype(auto)
      {
        auto copy = *this;
        operator ++();
        return copy;
      }

      friend constexpr auto operator ==(forward_iterator const& a,
                                        forward_iterator const& b) noexcept -> bool
      {
        return a.current == b.current;
      }

      friend constexpr auto operator !=(forward_iterator const& a,
                                        forward_iterator const& b) noexcept -> bool
      {
        return a.current != b.current;
      }
    };

    using iterator = forward_iterator<false>;

    using const_iterator = forward_iterator<true>;

    explicit pair(object const& = unit, object const& = unit);

    template <typename... Ts, typename = std::enable_if_t<(1 < sizeof...(Ts))>>
    explicit pair(object const& a, Ts&&... xs)
      : pair { a, make<pair>(std::forward<decltype(xs)>(xs)...) }
    {}

    virtual ~pair() = default;

    virtual auto compare(pair const*) const -> bool;

    virtual auto type() const noexcept -> std::type_info const&;

    virtual auto write(std::ostream &) const -> std::ostream &;

    constexpr auto begin() noexcept
    {
      return iterator(this);
    }

    constexpr auto begin() const noexcept
    {
      return const_iterator(this);
    }

    constexpr auto end() noexcept
    {
      return iterator(nullptr);
    }

    constexpr auto end() const noexcept
    {
      return const_iterator(nullptr);
    }

    constexpr auto cbegin() const -> const_iterator
    {
      return std::as_const(*this).begin();
    }

    constexpr auto cend() const noexcept
    {
      return std::as_const(*this).end();
    }
  };

  auto operator <<(std::ostream &, pair const&) -> std::ostream &;

  template <typename T, typename U, REQUIRES(std::is_constructible<pair, T, U>)>
  auto operator |(T&& x, U&& y) -> decltype(auto)
  {
    return make<pair>(std::forward<decltype(x)>(x),
                      std::forward<decltype(y)>(y));
  }

  inline auto cons = [](auto&&... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ...);
  };

  inline auto xcons = [](auto&& x, auto&& y) constexpr
  {
    return cons(std::forward<decltype(y)>(y),
                std::forward<decltype(x)>(x));
  };

  template <auto N, typename T>
  auto get(T&& x) -> decltype(auto)
  {
    if constexpr (std::is_same_v<std::decay_t<T>, pair::iterator>)
    {
      return std::get<N>(*x.current);
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

  inline constexpr auto caddddr = compose(car, cddddr);
} // namespace kernel
} // namespace meevax

template <>
struct std::hash<meevax::object>
{
  auto operator ()(meevax::object const& x) const noexcept
  {
    return std::hash<decltype(x.get())>()(x.get());
  }
};

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
