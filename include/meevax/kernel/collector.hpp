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

  /*
     This mark-and-sweep garbage collector is based on the implementation of
     gc_ptr written by William E. Kempf and posted to CodeProject.

     - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
     - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
  */
  template <typename...>
  struct collector
  {
    struct mutator;

    struct pair : public std::pair<mutator, mutator>
    {
      pair() = default;

      template <typename T,
                typename U = std::nullptr_t,
                typename = std::enable_if_t<std::is_constructible_v<std::pair<mutator, mutator>, T, U>>>
      explicit pair(T&& x, U&& y = nullptr)
        : std::pair<mutator, mutator> { std::forward<decltype(x)>(x), std::forward<decltype(y)>(y) }
      {}

      virtual ~pair() = default;

      virtual auto eqv(pair const* x) const -> bool
      {
        return static_cast<pair const*>(this) == x and static_cast<pair const&>(*this) == *x;
      }

      virtual auto extent() const noexcept -> std::pair<void const*, std::size_t>
      {
        return { static_cast<pair const*>(this), sizeof(pair) };
      }

      virtual auto contains(void const* p) const noexcept -> bool
      {
        auto base = static_cast<pair const*>(this);
        return base <= p and p < reinterpret_cast<void const*>(reinterpret_cast<std::uintptr_t>(base) + sizeof(pair));
      }

      virtual auto type() const noexcept -> std::type_info const&
      {
        return typeid(pair);
      }

      virtual auto write(std::ostream & o) const -> std::ostream &
      {
        return o << static_cast<pair const&>(*this);
      }
    };

    auto static inline cleared = false;

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
        if (not std::exchange(cleared, true))
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

      explicit constexpr binder(auto&&... xs)
        : std::conditional_t<std::is_base_of_v<pair, Bound> and std::is_constructible_v<pair, decltype(xs)...>, pair, Bound>(std::forward<decltype(xs)>(xs)...)
      {}

      explicit constexpr binder(with_braces_tag, auto&&... xs)
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

    using pointer = nan_boxing_pointer<pair, bool, small_integer, float, character, instruction>;

    struct mutator : public pointer
    {
      mutator(std::nullptr_t = nullptr) noexcept
      {}

      mutator(mutator const& other)
        : pointer { other }
      {
        if (*this)
        {
          assert(not mutators.contains(this));
          mutators.insert(this);
        }
      }

      mutator(pair * pair)
        : pointer { pair }
      {
        if (pair)
        {
          assert(not mutators.contains(this));
          mutators.insert(this);
        }
      }

      template <any_of<bool, small_integer, float, double, character, instruction> T>
      mutator(T const& datum)
        : pointer { datum }
      {
        assert(pointer::get() == nullptr);
      }

      ~mutator()
      {
        if (pointer::operator bool() and not cleared)
        {
          assert(mutators.contains(this));
          mutators.erase(this);
        }
      }

      auto operator =(mutator const& other) -> auto &
      {
        reset(other);
        return *this;
      }

      auto operator =(std::nullptr_t) -> auto &
      {
        reset();
        return *this;
      }

      auto reset(mutator const& after) -> void
      {
        auto const before = pointer::operator bool();

        pointer::reset(after);

        if (before)
        {
          if (not after)
          {
            assert(mutators.contains(this));
            mutators.erase(this);
          }
        }
        else if (after)
        {
          assert(not mutators.contains(this));
          mutators.insert(this);
        }
      }

      auto reset(std::nullptr_t = nullptr) -> void
      {
        auto const before = pointer::operator bool();

        pointer::reset();

        if (before)
        {
          assert(mutators.contains(this));
          mutators.erase(this);
        }
      }

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

      auto eqv(mutator const& rhs) const -> bool
      {
        if (pointer::dereferenceable())
        {
          return *this ? pointer::unsafe_get()->eqv(rhs.get()) : rhs.is<std::nullptr_t>();
        }
        else
        {
          return pointer::compare(rhs);
        }
      }

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

      auto type() const -> std::type_info const&
      {
        if (pointer::dereferenceable())
        {
          return *this ? pointer::unsafe_get()->type() : typeid(std::nullptr_t);
        }
        else
        {
          return pointer::type();
        }
      }

      auto write(std::ostream & os) const -> std::ostream &
      {
        if (pointer::dereferenceable())
        {
          return *this ? pointer::unsafe_get()->write(os) : os << magenta("()");
        }
        else
        {
          return pointer::write(os);
        }
      }

      auto friend operator <<(std::ostream & os, mutator const& datum) -> std::ostream &
      {
        return datum.write(os);
      }
    };

    /*
       https://www.kernel.org/doc/html/latest/arch/x86/x86_64/mm.html

       0x0000'0000'0000'0000 ~ 0x0000'7FFF'FFFF'FFFF
    */
    template <typename T>
    using pointer_set = memory::pointer_set<T const*, std::bit_width(0x7FFFu),
                                                      std::bit_width(0xFFFFu),
                                                      std::bit_width(0xFFFFu)>;

    static inline pointer_set<pair> objects {};

    static inline pointer_set<mutator> mutators {};

    static inline std::size_t size = 0;

    static inline std::size_t threshold = 16_MiB;

    collector() = delete;

    collector(collector &&) = delete;

    collector(collector const&) = delete;

    ~collector() = delete;

    auto operator =(collector &&) -> collector & = delete;

    auto operator =(collector const&) -> collector & = delete;

    template <typename T, typename A>
    auto static make(auto&&... xs) -> mutator
    {
      if constexpr (std::is_class_v<T>)
      {
        if (size += sizeof(T); threshold < size)
        {
          collect();
        }

        if (auto data = new binder<T, A>(std::forward<decltype(xs)>(xs)...); data)
        {
          objects.insert(data);

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
        return make<T, default_allocator>(std::forward<decltype(xs)>(xs)...);
      }

      template <typename A>
      struct maker_with_custom_allocator
      {
        auto operator ()(auto&&... xs) const -> decltype(auto)
        {
          return make<T, A>(std::forward<decltype(xs)>(xs)...);
        }
      };

      template <typename A>
      auto static inline constexpr with = maker_with_custom_allocator<A>();
    };

    auto static clear() -> void
    {
      for (auto const& object : objects)
      {
        delete object;
        assert(objects.contains(object));
        objects.erase(object);
      }
    }

    auto static collect() -> void
    {
      auto roots = pointer_set<mutator>();

      for (auto m : mutators)
      {
        if (is_root(m))
        {
          roots.insert(m);
        }
      }

      size = 0;

      auto reachables = pointer_set<pair>();

      for (auto root : roots)
      {
        auto static stack = std::vector<pair const*>();

        auto mark = [&](auto m)
        {
          assert(m);
          assert(m->unsafe_get());

          if (auto p = m->unsafe_get(); not reachables.contains(p))
          {
            reachables.insert(p);
            objects.erase(p);
            stack.push_back(p);
          }
        };

        mark(root);

        while (not stack.empty())
        {
          auto [base, size_] = stack.back()->extent();

          stack.pop_back();

          size += size_;

          std::for_each(mutators.lower_bound(reinterpret_cast<mutator const*>(base)),
                        mutators.lower_bound(reinterpret_cast<mutator const*>(reinterpret_cast<std::uintptr_t>(base) + size_)),
                        mark);
        }
      }

      for (auto object : objects)
      {
        delete object;
      }

      objects.swap(reachables);

      threshold = std::max(threshold, size + (size / 2));
    }

    auto static count() noexcept -> std::size_t
    {
      return objects.size();
    }

    auto static is_root(mutator const* m) noexcept
    {
      /*
         If the given mutator is a non-root object, then an object containing
         this mutator as a data member exists somewhere in memory.

         Containing the mutator as a data member means that the address of the
         mutator is contained in the interval of the object's base-address ~
         base-address + object-size. The pair is present to keep track of the
         base-address and size of the object needed here.

         The memory layout of the base class pair and Bound of the binder is
         implementation-defined. That is, there is no guarantee that the
         pointer value of pair const* is less than the pointer value of Bound
         const*. Therefore, the iterator returned by lower_bound here points to
         pair const*, which may be an iterator to the object itself, which may
         contain m, or the next iterator of the object, which may contain m.
      */
      auto iterator = objects.lower_bound(reinterpret_cast<pair const*>(m));

      return not ((iterator and (*iterator)->contains(m)) or (--iterator and (*iterator)->contains(m)));
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_COLLECTOR_HPP
