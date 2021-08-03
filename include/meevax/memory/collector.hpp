/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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

    struct object
    {
    protected:
      explicit constexpr object() = default;

      explicit object(pointer<void> const derived, deallocator<void>::signature const deallocate)
      {
        if (auto const lock = std::unique_lock(resource); derived and lock)
        {
          objects.emplace(this, collector::reset(derived, deallocate));
        }
      }

      template <typename Pointer>
      explicit object(Pointer const derived)
        : object { derived, deallocator<typename std::pointer_traits<Pointer>::element_type>::deallocate }
      {}

      ~object()
      {
        if (auto const lock = std::unique_lock(resource); lock)
        {
          objects.erase(this);
        }
      }

      void reset(pointer<void> const derived, deallocator<void>::signature const deallocate)
      {
        if (auto const lock = std::unique_lock(resource); derived and lock)
        {
          objects[this] = collector::reset(derived, deallocate);
        }
      }

      template <typename Pointer>
      void reset(Pointer const derived)
      {
        reset(derived, deallocator<typename std::pointer_traits<Pointer>::element_type>::deallocate);
      }
    };

  private:
    static inline std::mutex resource;

    static inline std::map<pointer<object>, pointer<region>> objects;

    static inline std::set<pointer<region>> regions;

    static inline std::size_t allocation;

    static inline std::size_t threshold;

  public:
    explicit collector();

    explicit collector(collector &&) = delete;

    explicit collector(collector const&) = delete;

    ~collector();

           auto operator =(collector &&) -> collector & = delete;

           auto operator =(collector const&) -> collector & = delete;

           auto allocate(std::size_t const) -> pointer<void>;

           auto clear() -> void;

           auto collect() -> std::size_t;

           auto count() const noexcept -> std::size_t;

           auto deallocate(pointer<void> const, std::size_t const = 0) -> void;

           auto mark() -> void;

           auto overflow() const noexcept -> bool;

    static auto region_of(pointer<void> const) -> decltype(regions)::iterator;

    static auto reset(pointer<void> const, deallocator<void>::signature const) -> pointer<region>;

           void reset_threshold(std::size_t const = std::numeric_limits<std::size_t>::max());

           void sweep();

           void traverse(pointer<region> const);
  } static gc;
} // namespace memory
} // namespace meevax

auto operator new(std::size_t const, meevax::collector &) -> meevax::pointer<void>;

void operator delete(meevax::pointer<void> const, meevax::collector &) noexcept;

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
