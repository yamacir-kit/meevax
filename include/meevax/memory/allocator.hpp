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

#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility> // std::exchange

namespace meevax::inline memory
{
/*
   Simple Segregated Storage Allocator
*/
template <typename T, typename Capacity = std::integral_constant<std::size_t, 1024>>
class allocator
{
  union chunk
  {
    chunk * tail;

    T value;
  };

  struct block
  {
    std::uint8_t data[sizeof(chunk) * Capacity::value] = {};

    std::size_t size = Capacity::value;

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
      return data + sizeof(chunk) * --size;
    }
  };

  static inline chunk * free_list = nullptr;

  static inline block * free_space = new block();

  static inline std::size_t count = 0;

public:
  using value_type = T;

  using propagate_on_container_move_assignment = std::false_type;

  using size_type = std::size_t;

  using difference_type = std::ptrdiff_t;

  using is_always_equal = std::false_type;

  template <typename U>
  struct rebind
  {
    using other = allocator<U, Capacity>;
  };

  explicit allocator()
  {
    ++count;
  }

  allocator(allocator &&)
  {
    ++count;
  }

  allocator(allocator const&)
  {
    ++count;
  }

  ~allocator()
  {
    if (not --count)
    {
      delete free_space;
    }
  }

  auto operator =(allocator &&) -> allocator & = default;

  auto operator =(allocator const&) -> allocator & = default;

  static auto allocate(size_type = 1)
  {
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

  static auto deallocate(value_type * p, size_type = 1) -> void
  {
    reinterpret_cast<chunk *>(p)->tail = free_list;
    free_list = reinterpret_cast<chunk *>(p);
  }
};
} // namespace meevax::memory

#endif // INCLUDED_MEEVAX_MEMORY_ALLOCATOR_HPP
