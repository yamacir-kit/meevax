/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_MEMORY_ALLOCATOR_HPP
#define INCLUDED_MEEVAX_MEMORY_ALLOCATOR_HPP

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility> // std::exchange

namespace meevax::inline memory
{
  template <typename T, std::size_t N = 1024>
  struct segregated_storage_allocator
  {
    using value_type = T;

    using propagate_on_container_move_assignment = std::false_type;

    using size_type = std::size_t;

    using difference_type = std::ptrdiff_t;

    using is_always_equal = std::false_type;

    template <typename U>
    struct rebind
    {
      using other = segregated_storage_allocator<U, N>;
    };

    union chunk
    {
      chunk * tail;

      T value;

      ~chunk()
      {}
    };

    struct block
    {
      chunk data[N] = {};

      size_type size = N;

      block * const tail;

      explicit constexpr block(block * tail = nullptr)
        : tail { tail }
      {}

      ~block()
      {
        delete tail;
      }

      auto pop()
      {
        return &data[--size];
      }
    };

    static inline chunk * free_list = nullptr;

    static inline block * free_space = new block();

    static inline size_type count = 0;

    explicit segregated_storage_allocator()
    {
      ++count;
    }

    segregated_storage_allocator(segregated_storage_allocator &&)
    {
      ++count;
    }

    segregated_storage_allocator(segregated_storage_allocator const&)
    {
      ++count;
    }

    ~segregated_storage_allocator()
    {
      if (not --count)
      {
        delete free_space;
      }
    }

    auto operator =(segregated_storage_allocator &&) -> segregated_storage_allocator & = default;

    auto operator =(segregated_storage_allocator const&) -> segregated_storage_allocator & = default;

    auto static allocate([[maybe_unused]] size_type n = 1)
    {
      assert(n == 1);

      if (free_list)
      {
        return reinterpret_cast<value_type *>(std::exchange(free_list, free_list->tail));
      }
      else
      {
        if (not free_space->size)
        {
          free_space = new block(free_space);
        }

        return reinterpret_cast<value_type *>(free_space->pop());
      }
    }

    auto static deallocate(value_type * p, [[maybe_unused]] size_type n = 1) -> void
    {
      assert(n == 1);

      reinterpret_cast<chunk *>(p)->tail = std::exchange(free_list, reinterpret_cast<chunk *>(p));
    }
  };
} // namespace meevax::memory

#endif // INCLUDED_MEEVAX_MEMORY_ALLOCATOR_HPP
