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

#include <cstddef>
#include <memory>
#include <unordered_map>

#include <meevax/memory/literal.hpp>
#include <meevax/memory/marker.hpp>
#include <meevax/memory/integer_set.hpp>

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
    struct tag : public marker
    {
      std::uintptr_t address;

      std::size_t size;

      explicit tag(void const* const address, std::size_t size)
        : address { reinterpret_cast<std::uintptr_t>(address) }
        , size    { size }
      {}

      virtual ~tag() = default;

      template <typename T = void>
      auto lower_address() const noexcept
      {
        return reinterpret_cast<T *>(address);
      }

      template <typename T = void>
      auto upper_address() const noexcept
      {
        return reinterpret_cast<T *>(address + size);
      }

      auto contains(void const* const data) const noexcept
      {
        return lower_address() <= data and data < upper_address();
      }
    };

    template <typename T, typename AllocatorTraits>
    struct tagged : public tag
    {
      using allocator_type = typename AllocatorTraits::template rebind_alloc<tagged<T, AllocatorTraits>>;

      using pointer = typename std::allocator_traits<allocator_type>::pointer;

      /*
         Support for custom allocators is incomplete. Because the allocator is
         templated and held as a static data member, this allocator may be
         constructed after the collector and destructed before the collector.
         (See "Static Initialization/Destruction Order Fiasco.") In that case,
         illegal memory accesses will occur because the allocator corresponding
         to a particular type has already been destructed at the time
         collector::clear() is called within collector::~collector().

         The problem is currently not occurring because the lifetime of the
         storage allocated by std::allocator is independent of the lifetime of
         the std::allocator type object. Conversely, if there is a relationship
         between the storage allocated by the allocator object and the lifetime
         of the allocator object, a problem will occur. For example, a memory
         pool allocator, which allocates the pool in the allocator constructor
         and releases the pool in the allocator destructor, will cause
         problems.
      */
      static inline auto allocator = allocator_type();

      T body;

      template <typename... Ts>
      explicit tagged(Ts&&... xs)
        : tag  { std::addressof(body), sizeof(T) }
        , body { std::forward<decltype(xs)>(xs)... }
      {}

      ~tagged() override = default;

      auto operator new(std::size_t) -> void *
      {
        return allocator.allocate(1);
      }

      auto operator delete(void * data) noexcept -> void
      {
        allocator.deallocate(reinterpret_cast<pointer>(data), 1);
      }
    };

    class mutator
    {
      friend class collector;

    protected:
      tag * object = nullptr;

      explicit constexpr mutator() = default;

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
        else if (cache->contains(data)) // Heuristic-based optimization.
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

  protected:
    static inline tag * cache = nullptr;

    static inline integer_set<tag *, 16, 16, 16> tags {};

    static inline integer_set<mutator *, 16, 16, 16> mutators {};

    static inline std::size_t allocation = 0;

    static inline std::size_t threshold = 8_MiB;

    static inline std::unordered_map<std::string, std::unique_ptr<void, void (*)(void * const)>> dynamic_linked_libraries {};

  public:
    collector() = default;

    collector(collector &&) = delete;

    collector(collector const&) = delete;

    ~collector();

    auto operator =(collector &&) -> collector & = delete;

    auto operator =(collector const&) -> collector & = delete;

    template <typename T, typename Allocator = default_allocator<void>, typename... Ts>
    static auto make(Ts&&... xs)
    {
      if (allocation += sizeof(T); threshold < allocation)
      {
        collect();
      }

      if (auto data = new tagged<T, std::allocator_traits<Allocator>>(std::forward<decltype(xs)>(xs)...); data)
      {
        tags.insert(cache = data);

        return std::addressof(data->body);
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

    static auto mark() -> void;

    static auto mark(tag * const) -> void;

    static auto sweep() -> void;
  };

  auto primary_collector() -> collector &;
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
