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
  /* ---- Acknowledgement ------------------------------------------------------
   *
   *  This mark-and-sweep garbage collector is based on the implementation of
   *  gc_ptr written by William E. Kempf and posted to CodeProject.
   *
   *  - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
   *  - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
   *
   * ------------------------------------------------------------------------ */
  class collector
  {
  public:
    using is_always_equal = std::true_type;

    struct interior
    {
    protected:
      explicit constexpr interior() = default;

      template <typename Pointer>
      explicit interior(Pointer const p)
      {
        if (p)
        {
          auto const lock = std::unique_lock(resource);
          objects.try_emplace(this, collector::reset(p, deallocator<Pointer>::deallocate));
        }
      }

      ~interior()
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

    template <typename T>
    using fast_set = std::set<T, std::less<T>, simple_allocator<T>>;

    static inline fast_set<region *> regions;

    template <typename T, typename U>
    using fast_map = std::map<T, U, std::less<T>, simple_allocator<std::pair<T, U>>>;

    static inline fast_map<interior * const, region *> objects;

    static inline std::size_t allocation;

    static inline std::size_t threshold;

  public:
    explicit collector();

    explicit collector(collector &&) = delete;

    explicit collector(collector const&) = delete;

    ~collector();

    auto operator =(collector &&) -> collector & = delete;

    auto operator =(collector const&) -> collector & = delete;

    auto allocate(std::size_t const) -> void *;

    auto clear() -> void;

    auto collect() -> std::size_t;

    auto count() const noexcept -> std::size_t;

    auto deallocate(void * const, std::size_t const = 0) -> void;

    auto mark() -> void;

    static auto region_of(void const* const) -> decltype(regions)::iterator;

    static auto reset(void * const, deallocator<void>::signature const) -> region *;

    auto reset_threshold(std::size_t const = std::numeric_limits<std::size_t>::max()) -> void;

    auto sweep() -> void;

    auto traverse(region * const) -> void;
  } static gc;
} // namespace memory
} // namespace meevax

auto operator new(std::size_t const, meevax::collector &) -> void *;

void operator delete(void * const, meevax::collector &) noexcept;

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
