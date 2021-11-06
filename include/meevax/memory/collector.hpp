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

    struct object
    {
    protected:
      explicit constexpr object() = default;

      explicit object(const_void_pointer derived, deallocator<void>::signature const deallocate)
      {
        if (auto const lock = std::unique_lock(resource); derived and lock)
        {
          objects.try_emplace(this, collector::reset(derived, deallocate));
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

      void reset(const_void_pointer derived, deallocator<void>::signature const deallocate)
      {
        if (auto const lock = std::unique_lock(resource); derived and lock)
        {
          objects.insert_or_assign(this, collector::reset(derived, deallocate));
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

    static inline std::map<
      pointer_to<object>,
      pointer_to<region>,
      std::less<pointer_to<object>>,
      simple_allocator<std::pair<const_pointer_to<object>, pointer_to<region>>>
    > objects;

    static inline std::set<
      pointer_to<region>,
      std::less<pointer_to<region>>,
      simple_allocator<pointer_to<region>>
    > regions;

    static inline std::size_t allocation;

    static inline std::size_t threshold;

  public:
    explicit collector();

    explicit collector(collector &&) = delete;

    explicit collector(collector const&) = delete;

    ~collector();

           auto operator =(collector &&) -> collector & = delete;

           auto operator =(collector const&) -> collector & = delete;

           auto allocate(std::size_t const) -> void_pointer;

           auto clear() -> void;

           auto collect() -> std::size_t;

           auto count() const noexcept -> std::size_t;

           auto deallocate(const_void_pointer, std::size_t const = 0) -> void;

           auto mark() -> void;

           auto overflow() const noexcept -> bool;

    static auto region_of(const_void_pointer) -> decltype(regions)::iterator;

    static auto reset(const_void_pointer, deallocator<void>::signature const) -> pointer_to<region>;

           void reset_threshold(std::size_t const = std::numeric_limits<std::size_t>::max());

           void sweep();

           void traverse(pointer_to<region> const);
  } static gc;
} // namespace memory
} // namespace meevax

auto operator new(std::size_t const, meevax::collector &) -> meevax::void_pointer;

void operator delete(meevax::const_void_pointer, meevax::collector &) noexcept;

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
