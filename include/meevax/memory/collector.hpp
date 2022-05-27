/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#include <cassert>
#include <cstddef>
#include <limits>
#include <map>
#include <mutex>
#include <new>
#include <set>

#include <meevax/memory/region.hpp>
#include <meevax/memory/simple_allocator.hpp>
#include <meevax/string/header.hpp>

namespace meevax
{
inline namespace memory
{
  class collector /* -----------------------------------------------------------
  *
  *  This mark-and-sweep garbage collector is based on the implementation of
  *  gc_ptr written by William E. Kempf and posted to CodeProject.
  *
  *  - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
  *  - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
  *
  * ------------------------------------------------------------------------- */
  {
  public:
    struct collectable
    {
    protected:
      explicit constexpr collectable() = default;

      template <typename Pointer>
      explicit collectable(Pointer const p)
      {
        if (p)
        {
          auto const lock = std::unique_lock(resource);
          objects.try_emplace(this, collector::reset(p, deallocator<Pointer>::deallocate));
        }
      }

      ~collectable()
      {
        auto const lock = std::unique_lock(resource);
        objects.erase(this);
      }

      template <typename Pointer>
      void reset(Pointer const p)
      {
        if (p)
        {
          auto const lock = std::unique_lock(resource);
          objects.insert_or_assign(this, collector::reset(p, deallocator<Pointer>::deallocate));
        }
      }
    };

  private:
    static inline std::mutex resource;

    static inline simple_allocator<region> region_allocator {};

    template <typename T>
    using set = std::set<T, std::less<T>, simple_allocator<T>>;

    static inline set<region *> regions;

    template <typename T, typename U>
    using map = std::map<T, U, std::less<T>, simple_allocator<std::pair<T, U>>>;

    static inline map<collectable * const, region *> objects;

    static inline std::size_t allocation;

    static inline std::size_t threshold;

  public:
    explicit collector();

    explicit collector(collector &&) = delete;

    explicit collector(collector const&) = delete;

    ~collector();

    auto operator =(collector &&) -> collector & = delete;

    auto operator =(collector const&) -> collector & = delete;

    static auto allocate(std::size_t const) -> void *;

    static auto clear() -> void;

    static auto collect() -> std::size_t;

    static auto count() noexcept -> std::size_t;

    static auto deallocate(void * const, std::size_t const = 0) -> void;

    static auto mark() -> void;

    static auto region_of(void const* const) -> decltype(regions)::iterator;

    static auto reset(void * const, deallocator<void>::signature const) -> region *;

    static auto reset_threshold(std::size_t const = std::numeric_limits<std::size_t>::max()) -> void;

    static auto sweep() -> void;

    static auto traverse(region * const) -> void;
  } static gc;
} // namespace memory
} // namespace meevax

auto operator new(std::size_t const, meevax::collector &) -> void *;

void operator delete(void * const, meevax::collector &) noexcept;

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
