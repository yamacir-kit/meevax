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

#ifndef INCLUDED_MEEVAX_KERNEL_COLLECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_COLLECTOR_HPP

#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/object.hpp>
#include <meevax/memory/allocator.hpp>
#include <meevax/memory/literal.hpp>
#include <meevax/memory/pointer_set.hpp>
#include <memory> // std::allocator

namespace meevax::inline kernel
{
  inline constexpr struct with_braces_tag {} with_braces {};

    struct pair : public std::pair<object, object>
    {
      pair()
        : std::pair<object, object> { nullptr, nullptr }
      {}

      template <typename T,
                typename U = std::nullptr_t,
                typename = std::enable_if_t<std::is_constructible_v<std::pair<object, object>, T, U>>>
      explicit pair(T&& x, U&& y = nullptr)
        : std::pair<object, object> { std::forward<decltype(x)>(x), std::forward<decltype(y)>(y) }
      {}

      virtual ~pair() = default;

      auto virtual eqv(pair const*) const -> bool;

      auto virtual extent() const noexcept -> std::pair<void const*, std::size_t>;

      auto virtual contains(void const*) const noexcept -> bool;

      auto virtual type() const noexcept -> std::type_info const&;

      auto virtual write(std::ostream &) const -> std::ostream &;
    };

    auto clear() -> void;

    auto clear_once() -> void;

    template <typename A>
    struct stateful : public A
    {
      ~stateful()
      {
        /*
           Execute clear before any static allocator is destroyed. Otherwise,
           when the destructor of the collector executes clear, the collector
           may touch the freed memory of the stateful allocator.
        */
        clear_once();
      }
    };

    template <typename Bound, typename A>
    struct binder final : public virtual pair
                        , public Bound
    {
      using allocator = stateful<typename std::allocator_traits<A>::template rebind_alloc<binder>>;

      auto static inline a = allocator();

      explicit binder(auto&&... xs)
        : std::conditional_t<std::is_base_of_v<pair, Bound> and std::is_constructible_v<pair, decltype(xs)...>, pair, Bound>(std::forward<decltype(xs)>(xs)...)
      {}

      explicit binder(with_braces_tag, auto&&... xs)
        : std::conditional_t<std::is_base_of_v<pair, Bound> and std::is_constructible_v<pair, decltype(xs)...>, pair, Bound> { std::forward<decltype(xs)>(xs)... }
      {}

      ~binder() override = default;

      auto eqv([[maybe_unused]] pair const* other) const -> bool override
      {
        if constexpr (std::equality_comparable<Bound const&>)
        {
          if (auto const* bound = dynamic_cast<Bound const*>(other); bound)
          {
            return this == bound or *static_cast<Bound const*>(this) == *bound;
          }
          else
          {
            return std::is_same_v<Bound, std::nullptr_t>;
          }
        }
        else
        {
          return false;
        }
      }

      auto extent() const noexcept -> std::pair<void const*, std::size_t> override
      {
        return { static_cast<Bound const*>(this), sizeof(Bound) };
      }

      auto contains(void const* p) const noexcept -> bool override
      {
        auto base = static_cast<Bound const*>(this);
        return base <= p and p < reinterpret_cast<void const*>(reinterpret_cast<std::uintptr_t>(base) + sizeof(Bound));
      }

      auto type() const noexcept -> std::type_info const& override
      {
        return typeid(Bound);
      }

      auto write(std::ostream & os) const -> std::ostream & override
      {
        if constexpr (requires { os << static_cast<Bound const&>(*this); })
        {
          return os << static_cast<Bound const&>(*this);
        }
        else
        {
          return os << magenta("#,(") << green(typeid(Bound).name()) << faint(" #;", static_cast<Bound const*>(this)) << magenta(")");
        }
      }

      auto operator new(std::size_t) -> void *
      {
        return a.allocate(1);
      }

      auto operator delete(void * data) noexcept -> void
      {
        a.deallocate(reinterpret_cast<typename std::allocator_traits<allocator>::pointer>(data), 1);
      }
    };

    template <typename A>
    struct binder<pair, A> final : public pair
    {
      using allocator = stateful<typename std::allocator_traits<A>::template rebind_alloc<binder>>;

      auto static inline a = allocator();

      using pair::pair;

      ~binder() override = default;

      auto operator new(std::size_t) -> void *
      {
        return a.allocate(1);
      }

      auto operator delete(void * data) noexcept -> void
      {
        a.deallocate(reinterpret_cast<typename std::allocator_traits<allocator>::pointer>(data), 1);
      }
    };

    /*
       https://www.kernel.org/doc/html/latest/arch/x86/x86_64/mm.html

       0x0000'0000'0000'0000 ~ 0x0000'7FFF'FFFF'FFFF
    */
    template <typename T>
    using canonical_pointer_set = pointer_set<T const*, std::bit_width(0x7FFFu),
                                                        std::bit_width(0xFFFFu),
                                                        std::bit_width(0xFFFFu)>;

    auto collect() -> void;

    auto count() -> std::size_t;

    auto request(std::size_t) -> void;

    auto reserve(std::size_t) -> void;

    auto insert(pair const*) -> void;

    auto is_root(object const*) noexcept -> bool;

    template <typename T, typename A = std::conditional_t<std::is_same_v<T, pair>, segregated_storage_allocator<void>, std::allocator<void>>>
    auto make(auto&&... xs) -> object
    {
      if constexpr (std::is_class_v<T>)
      {
        request(sizeof(T));

        if (auto datum = new binder<T, A>(std::forward<decltype(xs)>(xs)...); datum)
        {
          insert(datum);

          return datum;
        }
        else [[unlikely]]
        {
          throw std::bad_alloc();
        }
      }
      else
      {
        return { std::forward<decltype(xs)>(xs)... };
      }
    }

namespace backdoor
{
  auto objects() -> canonical_pointer_set<object> const&;
}
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_COLLECTOR_HPP
