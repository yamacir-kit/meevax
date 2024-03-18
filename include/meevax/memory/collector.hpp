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

#ifndef INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
#define INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP

#include <cstddef> // std::size_t
#include <memory> // std::allocator
#include <unordered_map>

#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/memory/integer_set.hpp>
#include <meevax/memory/literal.hpp>
#include <meevax/memory/simple_allocator.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/type_traits/is_output_streamable.hpp>

namespace meevax
{
inline namespace memory
{
  /*
     This mark-and-sweep garbage collector is based on the implementation of
     gc_ptr written by William E. Kempf and posted to CodeProject.

     - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
     - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
  */
  class collector
  {
  public:
    struct top
    {
      virtual auto compare(top const*) const -> bool = 0;

      virtual auto type() const noexcept -> std::type_info const& = 0;

      virtual auto write(std::ostream &) const -> std::ostream & = 0;
    };

    template <typename Top, typename Bound>
    struct binder : public virtual Top
                  , public Bound
    {
      template <typename... Us>
      explicit constexpr binder(Us&&... xs)
        : std::conditional_t<std::is_base_of_v<Top, Bound> and std::is_constructible_v<Top, Us...>, Top, Bound> {
            std::forward<decltype(xs)>(xs)...
          }
      {}

      auto compare([[maybe_unused]] top const* other) const -> bool override
      {
        if constexpr (is_equality_comparable_v<Bound const&>)
        {
          if (auto const* bound = dynamic_cast<Bound const*>(other); bound)
          {
            return *bound == static_cast<Bound const&>(*this);
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

      auto type() const noexcept -> std::type_info const& override
      {
        return typeid(Bound);
      }

      auto write(std::ostream & os) const -> std::ostream & override
      {
        if constexpr (is_output_streamable_v<Bound const&>)
        {
          return os << static_cast<Bound const&>(*this);
        }
        else
        {
          return os << magenta("#,(") << green(typeid(Bound).name()) << faint(" #;", static_cast<Bound const*>(this)) << magenta(")");
        }
      }
    };

    struct tag
    {
      bool marked : 1;

      std::size_t size : 15;

      std::uintptr_t address : 48;

      explicit tag(void const* const address, std::size_t size)
        : marked  { false }
        , size    { size }
        , address { reinterpret_cast<std::uintptr_t>(address) }
      {
        assert(size < (1u << 15));
      }

      virtual ~tag() = default;

      auto lower_address() const noexcept
      {
        return address;
      }

      auto upper_address() const noexcept
      {
        return address + size;
      }

      auto contains(void const* const data) const noexcept
      {
        return reinterpret_cast<void const*>(lower_address()) <= data and data < reinterpret_cast<void const*>(upper_address());
      }
    };

    static inline auto cleared = false;

    template <typename T, typename AllocatorTraits>
    struct tagged : public tag
    {
      struct allocator_type : public AllocatorTraits::template rebind_alloc<tagged<T, AllocatorTraits>>
      {
        ~allocator_type()
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

      static inline auto allocator = allocator_type();

      T value;

      template <typename... Ts>
      explicit tagged(Ts&&... xs)
        : tag   { std::addressof(value), sizeof(T) }
        , value { std::forward<decltype(xs)>(xs)... }
      {}

      ~tagged() override = default;

      auto operator new(std::size_t) -> void *
      {
        return allocator.allocate(1);
      }

      auto operator delete(void * data) noexcept -> void
      {
        using pointer = typename std::allocator_traits<allocator_type>::pointer;
        allocator.deallocate(reinterpret_cast<pointer>(data), 1);
      }
    };

    class mutator
    {
      friend class collector;

    protected:
      tag * object = nullptr;

      constexpr mutator() = default;

      explicit mutator(tag * object) noexcept
        : object { object }
      {
        if (object)
        {
          mutators.insert(this);
        }
      }

      ~mutator() noexcept
      {
        if (object)
        {
          mutators.erase(this);
        }
      }

      auto reset(tag * after = nullptr) noexcept -> void
      {
        if (auto before = std::exchange(object, after); not before and after)
        {
          mutators.insert(this);
        }
        else if (before and not after)
        {
          mutators.erase(this);
        }
      }

      static auto locate(void * const data) noexcept -> tag *
      {
        if (not data)
        {
          return nullptr;
        }
        else if (cache->contains(data))
        {
          return cache;
        }
        else if (auto iter = tags.lower_bound(reinterpret_cast<tag *>(data)); iter != tags.begin() and (*--iter)->contains(data))
        {
          return *iter;
        }
        else
        {
          return nullptr;
        }
      }
    };

    template <typename... Ts>
    using default_allocator = std::allocator<Ts...>;

  private:
    static inline tag * cache = nullptr;

    /*
       0x0000'0000'0000'0000 ~ 0x7FFF'FFFF'FFFF'FFFF
    */
    static inline integer_set<tag *, 15, 16, 16> tags {};

    static inline integer_set<mutator *, 15, 16, 16> mutators {};

    static inline std::size_t allocation = 0;

    static inline std::size_t threshold = 8_MiB;

    static inline std::unordered_map<std::string, std::unique_ptr<void, void (*)(void * const)>> dynamic_linked_libraries {};

  public:
    collector();

    collector(collector &&) = delete;

    collector(collector const&) = delete;

    ~collector();

    auto operator =(collector &&) -> collector & = delete;

    auto operator =(collector const&) -> collector & = delete;

    template <typename T, typename Allocator = default_allocator<void>, typename... Ts>
    static auto make(Ts&&... xs)
    {
      static_assert(std::is_base_of_v<top, T>);

      if (allocation += sizeof(T); threshold < allocation)
      {
        collect();
      }

      if (auto data = new tagged<T, std::allocator_traits<Allocator>>(std::forward<decltype(xs)>(xs)...); data)
      {
        tags.insert(cache = data);

        return std::addressof(data->value);
      }
      else
      {
        throw std::bad_alloc();
      }
    }

    static auto clear() -> void;

    static auto collect() -> void;

    static auto count() noexcept -> std::size_t;

    static auto dlclose(void * const) -> void;

    static auto dlopen(std::string const&) -> void *;

    static auto dlsym(std::string const&, void * const) -> void *;

    static auto mark() noexcept -> void;

    static auto mark(tag * const) noexcept -> void;

    static auto sweep() -> void;
  };

  static collector default_collector {};
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
