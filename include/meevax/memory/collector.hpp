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

#include <meevax/memory/header.hpp>
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
      header * context = nullptr;

      explicit constexpr collectable() = default;

      template <typename Pointer>
      explicit collectable(Pointer const p)
        : collectable { p ? *header_of(p) : nullptr }
      {}

      explicit collectable(header * header)
        : context { header }
      {
        if (context)
        {
          auto const lock = std::unique_lock(resource);
          objects.try_emplace(this, context);
        }
      }

      ~collectable()
      {
        auto const lock = std::unique_lock(resource);
        objects.erase(this);
      }

      template <typename Pointer>
      auto reset(Pointer const p) -> void
      {
        reset(p ? *header_of(p) : nullptr);
      }

      auto reset(header * header = nullptr) -> void
      {
        if (context = header)
        {
          auto const lock = std::unique_lock(resource);
          objects.insert_or_assign(this, context);
        }
      }
    };

  private:
    static inline std::mutex resource;

    static inline simple_allocator<header> header_allocator {};

    template <typename T>
    using set = std::set<T, std::less<T>, simple_allocator<T>>;

    static inline set<header *> headers;

    template <typename T, typename U>
    using map = std::map<T, U, std::less<T>, simple_allocator<std::pair<T, U>>>;

    static inline map<collectable * const, header *> objects;

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
    static auto allocate(Ts&&... xs) -> T *
    {
      if (auto data = new T(std::forward<decltype(xs)>(xs)...); data)
      {
        if (threshold < allocation)
        {
          collect();
        }

        allocation += sizeof(T);

        headers.insert(header_allocator.new_(data, sizeof(T), deallocator<T *>::deallocate));

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

    static auto deallocate(void * const, std::size_t const = 0) -> void;

    static auto mark() -> void;

    static auto header_of(void const* const) -> decltype(headers)::iterator;

    static auto reset_threshold(std::size_t const = std::numeric_limits<std::size_t>::max()) -> void;

    static auto sweep() -> void;

    static auto traverse(header * const) -> void;
  } static gc;
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
