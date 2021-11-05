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

#ifndef INCLUDED_MEEVAX_MEMORY_POOL_ALLOCATOR_HPP
#define INCLUDED_MEEVAX_MEMORY_POOL_ALLOCATOR_HPP

#include <cstddef>
#include <meevax/memory/literal.hpp>
#include <meevax/memory/pointer.hpp>

namespace meevax
{
inline namespace memory
{
template <typename T, auto Capacity = 1024>
class pool_allocator
{
  struct chunk
  {
    pointer<chunk> next;
  };

  class chunks
  {
    static constexpr auto chunk_size = std::max(sizeof(T), sizeof(chunk));

    std::array<std::uint8_t, chunk_size * Capacity> data;

    std::size_t size = Capacity;

  public:
    const_pointer<chunks> next;

    explicit constexpr chunks(pointer<chunks> next = nullptr)
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
      return reinterpret_cast<pointer<value_type>>(std::addressof(data[chunk_size * --size]));
    }
  };

  pointer<chunk> recycled_chunk = nullptr;

  pointer<chunks> fresh_chunks;

public:
  using value_type = T;

  template <typename U>
  struct rebind
  {
    using other = pool_allocator<U, Capacity>;
  };

  explicit pool_allocator()
    : fresh_chunks { new chunks() }
  {}

  pool_allocator(pool_allocator &&) = delete;

  pool_allocator(pool_allocator const&) = delete;

  auto operator =(pool_allocator &&) -> pool_allocator & = delete;

  auto operator =(pool_allocator const&) -> pool_allocator & = delete;

  ~pool_allocator()
  {
    delete fresh_chunks;
  }

  auto allocate(std::size_t = 1)
  {
    if (recycled_chunk)
    {
      return reinterpret_cast<pointer<value_type>>(std::exchange(recycled_chunk, recycled_chunk->next));
    }

    if (not (*fresh_chunks).remaining())
    {
      fresh_chunks = new chunks(fresh_chunks);
    }

    return (*fresh_chunks).pop();
  }

  auto deallocate(pointer<value_type> p, std::size_t = 1) -> void
  {
    reinterpret_cast<pointer<chunk>>(p)->next = recycled_chunk;
    recycled_chunk = reinterpret_cast<pointer<chunk>>(p);
  }

  auto construct(pointer<value_type> p, value_type const& value) -> void
  {
    new (p) T(value);
  }

  auto destroy(pointer<value_type> p) -> void
  {
    (*p).~T();
  }
};
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_POOL_ALLOCATOR_HPP
