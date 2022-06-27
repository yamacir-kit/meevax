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
    class traceable
    {
      friend class collector;

      memory::tracer * tracer = nullptr;

      explicit traceable(memory::tracer * tracer)
        : tracer { tracer }
      {
        if (tracer)
        {
          auto const lock = std::unique_lock(resource);
          traceables.emplace(this);
        }
      }

      auto reset(memory::tracer * after) -> void
      {
        if (auto before = std::exchange(tracer, after); not before and after)
        {
          auto const lock = std::unique_lock(resource);
          traceables.emplace(this);
        }
        else if (before and not after)
        {
          traceables.erase(this);
        }
      }

      auto locate(void * const data)
      {
        assert(data);

        if (newest_tracer->contains(data)) // Heuristic-based optimization.
        {
          return newest_tracer;
        }
        else
        {
          auto dummy = memory::tracer(data, 0);
          auto iter = tracers.lower_bound(&dummy);
          assert(iter != std::end(tracers));
          return *iter;
        }
      }

    protected:
      explicit traceable() = default;

      explicit traceable(traceable const& other)
        : traceable { other.tracer }
      {}

      template <typename Pointer>
      explicit traceable(Pointer const p)
        : traceable { p ? locate(p) : nullptr }
      {}

      ~traceable()
      {
        if (tracer)
        {
          auto const lock = std::unique_lock(resource);
          traceables.erase(this);
        }
      }

      auto reset()
      {
        reset(nullptr);
      }

      template <typename Pointer>
      auto reset(Pointer const p) -> void
      {
        reset(p ? locate(p) : nullptr);
      }

      auto reset(traceable const& other) -> void
      {
        reset(other.tracer);
      }
    };

  private:
    static inline std::mutex resource;

    static inline simple_allocator<tracer> tracer_source {};

    static inline tracer * newest_tracer = nullptr;

    template <typename T>
    using set = std::set<T, std::less<T>, simple_allocator<T>>;

    static inline set<tracer *> tracers {};

    static inline set<traceable *> traceables {};

    static inline std::size_t allocation = 0;

    static inline std::size_t threshold = 8_MiB;

  public:
    explicit collector();

    explicit collector(collector &&) = delete;

    explicit collector(collector const&) = delete;

    ~collector();

    auto operator =(collector &&) -> collector & = delete;

    auto operator =(collector const&) -> collector & = delete;

    template <typename T, typename... Ts>
    static auto make(Ts&&... xs)
    {
      if (auto data = new T(std::forward<decltype(xs)>(xs)...); data)
      {
        if (allocation += sizeof(T); threshold < allocation)
        {
          collect();
        }

        newest_tracer = tracer_source.new_(data, sizeof(T), deallocator<T>::deallocate);

        [[maybe_unused]] auto [iter, success] = tracers.insert(newest_tracer);

        assert(success);

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

    static auto tracer_of(void * const) -> decltype(tracers)::iterator;

    static auto reset_threshold(std::size_t const = std::numeric_limits<std::size_t>::max()) -> void;

    static auto sweep() -> void;

    static auto trace(tracer * const) -> void;
  }
  static gc;
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
