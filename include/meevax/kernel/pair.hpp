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

#include <meevax/kernel/heterogeneous.hpp>
#include <meevax/kernel/instruction.hpp>

namespace meevax
{
inline namespace kernel
{
  struct pair;

  using object = heterogeneous<gc_pointer, pair, bool, std::int32_t, std::uint32_t, float, instruction>;

  using let = object;

  let extern unit;

  template <typename T, typename... Ts>
  auto make(Ts&&... xs)
  {
    return object::allocate<T>(std::forward<decltype(xs)>(xs)...);
  }

  template <typename T>
  auto make(T&& x)
  {
    return object::allocate<std::decay_t<T>>(std::forward<decltype(x)>(x));
  }

  template <template <typename...> typename Template, typename... Ts, REQUIRES(std::is_constructible<Template<Ts...>, Ts...>)>
  auto make(Ts&&... xs) -> decltype(auto)
  {
    return make<Template<Ts...>>(std::forward<decltype(xs)>(xs)...);
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
        if (current = current->second.get(); current == initial)
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

    virtual auto operator [](std::size_t) const -> object const&;

    virtual auto operator [](std::size_t) -> object &;

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
