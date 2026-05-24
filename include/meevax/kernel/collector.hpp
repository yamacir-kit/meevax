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

#include <concepts>
#include <memory> // std::allocator
#include <vector>

#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/character.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/memory/allocator.hpp>
#include <meevax/memory/literal.hpp>
#include <meevax/memory/nan_boxing_pointer.hpp>
#include <meevax/memory/pointer_set.hpp>

namespace meevax::inline kernel
{
  using null = std::nullptr_t;

  using small_integer = std::int32_t; // Fixed sized integer that can be boxed.
  using widen_integer = std::int64_t; // Fixed sized integer that is temporarily widened to prevent possible overflow.

  inline constexpr struct with_braces_tag {} with_braces {};

  template<typename T, typename... Ts>
  concept any_of = (std::same_as<T, Ts> or ...);

    struct pair;

    struct mutator : public nan_boxing_pointer<pair, bool, small_integer, float, character, instruction>
    {
      using pointer = nan_boxing_pointer<pair, bool, small_integer, float, character, instruction>;

      mutator(std::nullptr_t = nullptr) noexcept;

      mutator(mutator const&);

      mutator(pair *);

      template <any_of<bool, small_integer, float, double, character, instruction> T>
      mutator(T const& datum)
        : pointer { datum }
      {
        assert(pointer::get() == nullptr);
      }

      ~mutator();

      auto operator =(mutator const&) -> mutator &;

      auto operator =(std::nullptr_t) -> mutator &;

      auto reset(mutator const&) -> void;

      auto reset(std::nullptr_t = nullptr) -> void;

      template <typename U>
      auto static as(auto&& m) -> decltype(auto)
      {
        if constexpr (std::is_same_v<std::decay_t<U>, pair>)
        {
          return *m;
        }
        else if constexpr (std::is_class_v<std::decay_t<U>>)
        {
          if (auto data = dynamic_cast<std::add_pointer_t<U>>(m.get()); data)
          {
            return *data;
          }
          else
          {
            throw std::runtime_error("no viable conversion from " + demangle(m.type()) + " to " + demangle(typeid(U)));
          }
        }
        else
        {
          return m.pointer::template as<U>();
        }
      }

      template <typename U> auto as        ()       -> decltype(auto) { return as<U>                      (*this) ; }
      template <typename U> auto as        () const -> decltype(auto) { return as<U>                      (*this) ; }
      template <typename U> auto as_mutable() const -> decltype(auto) { return as<U>(const_cast<mutator &>(*this)); }

      auto eqv(mutator const&) const -> bool;

      template <typename U>
      auto is() const
      {
        return type() == typeid(std::decay_t<U>);
      }

      template <typename U, typename = std::enable_if_t<std::is_class_v<U>>>
      auto is_also() const
      {
        return dynamic_cast<std::add_pointer_t<U>>(pointer::get()) != nullptr;
      }

      auto type() const -> std::type_info const&;

      auto write(std::ostream &) const -> std::ostream &;

      auto friend operator <<(std::ostream & os, mutator const& datum) -> std::ostream &
      {
        return datum.write(os);
      }
    };

    struct pair : public std::pair<mutator, mutator>
    {
      pair()
        : std::pair<mutator, mutator> { nullptr, nullptr }
      {}

      template <typename T,
                typename U = std::nullptr_t,
                typename = std::enable_if_t<std::is_constructible_v<std::pair<mutator, mutator>, T, U>>>
      explicit pair(T&& x, U&& y = nullptr)
        : std::pair<mutator, mutator> { std::forward<decltype(x)>(x), std::forward<decltype(y)>(y) }
      {}

      virtual ~pair() = default;

      auto virtual eqv(pair const*) const -> bool;

      auto virtual extent() const noexcept -> std::pair<void const*, std::size_t>;

      auto virtual contains(void const*) const noexcept -> bool;

      auto virtual type() const noexcept -> std::type_info const&;

      auto virtual write(std::ostream &) const -> std::ostream &;
    };

    auto clear() -> void;

    auto cleared() -> bool &;

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
        if (not std::exchange(cleared(), true))
        {
          clear();
        }
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

    auto objects() -> canonical_pointer_set<pair> &; // TODO REMOVE THIS!!!

    auto size() -> std::size_t &;

    auto threshold() -> std::size_t &;

    template <typename T, typename A>
    auto static make_(auto&&... xs) -> mutator
    {
      if constexpr (std::is_class_v<T>)
      {
        if (size() += sizeof(T); threshold() < size())
        {
          collect();
        }

        if (auto data = new binder<T, A>(std::forward<decltype(xs)>(xs)...); data)
        {
          objects().insert(data);

          return data;
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

    template <typename T>
    struct maker
    {
      using default_allocator = std::conditional_t<std::is_same_v<T, pair>, segregated_storage_allocator<void>, std::allocator<void>>;

      auto operator ()(auto&&... xs) const -> decltype(auto)
      {
        return make_<T, default_allocator>(std::forward<decltype(xs)>(xs)...);
      }

      template <typename A>
      struct maker_with_custom_allocator
      {
        auto operator ()(auto&&... xs) const -> decltype(auto)
        {
          return make_<T, A>(std::forward<decltype(xs)>(xs)...);
        }
      };

      template <typename A>
      auto static inline constexpr with = maker_with_custom_allocator<A>();
    };

    auto count() -> std::size_t;

    auto is_root(mutator const*) noexcept -> bool;

    auto mutators() -> canonical_pointer_set<mutator> &; // TODO REMOVE THIS!!!
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_COLLECTOR_HPP
