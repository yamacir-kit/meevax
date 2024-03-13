/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_MEMORY_SIMPLE_ALLOCATOR_HPP
#define INCLUDED_MEEVAX_MEMORY_SIMPLE_ALLOCATOR_HPP

#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility> // std::exchange

namespace meevax
{
inline namespace memory
{
/*
   Simple Segregated Storage Allocator
*/
template <typename T, typename Capacity = std::integral_constant<std::size_t, 1024>>
class simple_allocator
{
  struct alignas(T) chunk
  {
    chunk * tail;
  };

  struct block
  {
    static constexpr auto chunk_size = std::max(sizeof(T), sizeof(chunk));

    std::uint8_t data[chunk_size * Capacity::value] = {};

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
      return data + chunk_size * --size;
    }
  };

  chunk * free_list = nullptr;

  block * free_space;

public:
  using value_type = T;

  using propagate_on_container_move_assignment = std::false_type;

  using size_type = std::size_t;

  using difference_type = std::ptrdiff_t;

  using is_always_equal = std::false_type;

  template <typename U>
  struct rebind
  {
    using other = simple_allocator<U, Capacity>;
  };

  explicit simple_allocator()
    : free_space { new block() }
  {}

  simple_allocator(simple_allocator &&) = delete;

  simple_allocator(simple_allocator const&) = delete;

  ~simple_allocator()
  {
    delete free_space;
  }

  auto operator =(simple_allocator &&) -> simple_allocator & = delete;

  auto operator =(simple_allocator const&) -> simple_allocator & = delete;

  auto allocate(size_type = 1)
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

  auto deallocate(value_type * p, size_type = 1) -> void
  {
    reinterpret_cast<chunk *>(p)->tail = free_list;
    free_list = reinterpret_cast<chunk *>(p);
  }
};
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_SIMPLE_ALLOCATOR_HPP
