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

#include <meevax/memory/tracer.hpp>
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
    struct traceable
    {
      friend class collector;

    protected:
      memory::tracer * tracer = nullptr;

      explicit constexpr traceable() = default;

      template <typename Pointer>
      explicit traceable(Pointer const p)
        : traceable { p ? *tracer_of(p) : nullptr }
      {}

      explicit traceable(memory::tracer * h)
        : tracer { h }
      {
        if (tracer)
        {
          auto const lock = std::unique_lock(resource);
          traceables.insert(this);
        }
      }

      ~traceable()
      {
        auto const lock = std::unique_lock(resource);
        traceables.erase(this);
      }

      template <typename Pointer>
      auto reset(Pointer const p) -> void
      {
        reset(p ? *tracer_of(p) : nullptr);
      }

      auto reset(memory::tracer * h = nullptr) -> void
      {
        if (not std::exchange(tracer, h))
        {
          auto const lock = std::unique_lock(resource);
          traceables.insert(this);
        }
      }
    };

  private:
    static inline std::mutex resource;

    template <typename T>
    using set = std::set<T, std::less<T>, simple_allocator<T>>;

    static inline set<tracer *> tracers;

    static inline set<traceable *> traceables;

    static inline std::size_t allocation;

    static inline std::size_t threshold;

  public:
    explicit collector();

    explicit collector(collector &&) = delete;

    explicit collector(collector const&) = delete;

    ~collector();

    auto operator =(collector &&) -> collector & = delete;

    auto operator =(collector const&) -> collector & = delete;

    template <typename T, typename... Ts>
    static auto allocate(Ts&&... xs)
    {
      if (auto data = new T(std::forward<decltype(xs)>(xs)...); data)
      {
        if (threshold < allocation)
        {
          collect();
        }

        allocation += sizeof(T);

        tracers.insert(new tracer(data, sizeof(T), deallocator<T>::deallocate));

        return data;
      }
      else
      {
        throw std::bad_alloc();
      }
    }

    static auto clear() -> void;

    static auto collect() -> std::size_t;

    static auto count() noexcept -> std::size_t;

    static auto mark() -> void;

    static auto tracer_of(void const* const) -> decltype(tracers)::iterator;

    static auto reset_threshold(std::size_t const = std::numeric_limits<std::size_t>::max()) -> void;

    static auto sweep() -> void;

    static auto trace(tracer * const) -> void;
  }
  static gc;
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
