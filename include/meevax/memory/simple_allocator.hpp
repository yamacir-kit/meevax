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

#include <algorithm> // std::max
#include <array>
#include <cstdint>
#include <type_traits>
#include <utility> // std::exchange

#include <meevax/memory/literal.hpp>

namespace meevax
{
inline namespace memory
{
template <typename T, auto Capacity = 1_KiB>
class simple_allocator
{
  struct chunk
  {
    chunk * next;
  };

  class chunks
  {
    static constexpr auto chunk_size = std::max(sizeof(T), sizeof(chunk));

    std::array<std::uint8_t, chunk_size * Capacity> data;

    std::size_t size = Capacity;

  public:
    chunks * const next;

    explicit constexpr chunks(chunks * next = nullptr)
      : next { next }
    {}

    ~chunks()
    {
      delete next;
    }

    auto remaining() const noexcept
    {
      return size;
    }

    auto pop()
    {
      return reinterpret_cast<pointer>(std::addressof(data[chunk_size * --size]));
    }
  };

  chunk * recycled_chunk = nullptr;

  chunks * fresh_chunks;

public:
  using value_type = T;

  using pointer = value_type *;

  using const_pointer = value_type const*;

  using reference = value_type &;

  using const_reference = value_type const&;

  using is_always_equal = std::false_type;

  template <typename U>
  struct rebind
  {
    using other = simple_allocator<U, Capacity>;
  };

  explicit simple_allocator()
    : fresh_chunks { new chunks() }
  {}

  simple_allocator(simple_allocator &&) = delete;

  simple_allocator(simple_allocator const&) = delete;

  auto operator =(simple_allocator &&) -> simple_allocator & = delete;

  auto operator =(simple_allocator const&) -> simple_allocator & = delete;

  ~simple_allocator()
  {
    delete fresh_chunks;
  }

  auto allocate(std::size_t = 1)
  {
    if (recycled_chunk)
    {
      return reinterpret_cast<pointer>(std::exchange(recycled_chunk, recycled_chunk->next));
    }
    else
    {
      if (not (*fresh_chunks).remaining())
      {
        fresh_chunks = new chunks(fresh_chunks);
      }

      return (*fresh_chunks).pop();
    }
  }

  auto deallocate(pointer p, std::size_t = 1) -> void
  {
    reinterpret_cast<chunk *>(p)->next = recycled_chunk;
    recycled_chunk = reinterpret_cast<chunk *>(p);
  }

  template <typename... Ts>
  auto new_(Ts&&... xs) // TODO Use std::allocator_traits<>::construct
  {
    return ::new (allocate()) T(std::forward<decltype(xs)>(xs)...);
  }

  auto delete_(pointer p) -> void // TODO Use std::allocator_traits<>::destroy
  {
    p->~T();
    deallocate(p);
  }
};
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_SIMPLE_ALLOCATOR_HPP
