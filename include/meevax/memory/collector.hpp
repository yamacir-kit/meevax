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

#include <cstddef>

#include <meevax/memory/header.hpp>
#include <meevax/memory/literal.hpp>
#include <meevax/memory/pointer_set.hpp>

namespace meevax
{
inline namespace memory
{
  /*
     This mark-and-sweep garbage collector is based on the implementation of
     `gc_ptr` written by William E. Kempf and posted to CodeProject.

     - https://www.codeproject.com/Articles/912/A-garbage-collection-framework-for-C
     - https://www.codeproject.com/Articles/938/A-garbage-collection-framework-for-C-Part-II
  */
  class collector
  {
  public:
    class registration
    {
      friend class collector;

      memory::header * header = nullptr;

      explicit registration(memory::header * header)
        : header { header }
      {
        if (header)
        {
          registry.insert(this);
        }
      }

      auto reset(memory::header * after) -> void
      {
        if (auto before = std::exchange(header, after); not before and after)
        {
          registry.insert(this);
        }
        else if (before and not after)
        {
          registry.erase(this);
        }
      }

      static auto locate(void * const data) -> memory::header *
      {
        assert(data);

        if (cache->contains(data)) // Heuristic-based optimization.
        {
          return cache;
        }
        else if (auto iter = headers.lower_bound(reinterpret_cast<memory::header *>(data)); iter != std::begin(headers) and (*--iter)->contains(data))
        {
          return *iter;
        }
        else
        {
          return nullptr;
        }
      }

    protected:
      explicit constexpr registration() = default;

      explicit registration(registration const& other)
        : registration { other.header }
      {}

      template <typename Pointer>
      explicit registration(Pointer const p)
        : registration { p ? locate(p) : nullptr }
      {}

      ~registration()
      {
        if (header)
        {
          registry.erase(this);
        }
      }

      auto reset()
      {
        reset(nullptr);
      }

      template <typename Pointer>
      auto reset(Pointer const p) -> void
      {
        reset(p != nullptr ? locate(p) : nullptr);
      }

      auto reset(registration const& other) -> void
      {
        reset(other.header);
      }
    };

  protected:
    static inline header * cache = nullptr;

    static inline pointer_set<header *> headers {};

    static inline pointer_set<registration *> registry {};

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
      if (allocation += sizeof(T); threshold < allocation)
      {
        collect();
      }

      if (auto data = new body<T>(std::forward<decltype(xs)>(xs)...); data)
      {
        headers.insert(cache = data);

        return std::addressof(data->object);
      }
      else
      {
        throw std::bad_alloc();
      }
    }

    static auto clear() -> void;

    static auto collect() -> void;

    static auto count() noexcept -> std::size_t;

    static auto mark() -> void;

    static auto mark(header * const) -> void;

    static auto sweep() -> void;
  }
  static gc;
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_COLLECTOR_HPP
