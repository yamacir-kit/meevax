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
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility> // std::exchange

namespace meevax
{
inline namespace memory
{
template <typename T, typename Capacity = std::integral_constant<std::size_t, 1024>>
class simple_allocator
{
  struct alignas(T) chunk
  {
    chunk * next;
  };

  class chunks
  {
    static constexpr auto chunk_size = std::max(sizeof(T), sizeof(chunk));

    std::array<std::uint8_t, chunk_size * Capacity::value> data;

    std::size_t size = Capacity::value;

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
      return reinterpret_cast<value_type *>(std::addressof(data[chunk_size * --size]));
    }
  };

  chunk * recycled_chunk = nullptr;

  chunks * fresh_chunks;

public:
  using value_type = T;

  using propagate_on_container_move_assignment = std::false_type;

  using size_type = std::size_t;

  using difference_type = std::ptrdiff_t;

  using is_always_equal = std::false_type;

  explicit simple_allocator()
    : fresh_chunks { new chunks() }
  {}

  simple_allocator(simple_allocator &&) = delete;

  simple_allocator(simple_allocator const&) = delete;

  ~simple_allocator()
  {
    delete fresh_chunks;
  }

  auto operator =(simple_allocator &&) -> simple_allocator & = delete;

  auto operator =(simple_allocator const&) -> simple_allocator & = delete;

  auto allocate(size_type = 1)
  {
    if (recycled_chunk)
    {
      return reinterpret_cast<value_type *>(std::exchange(recycled_chunk, recycled_chunk->next));
    }
    else
    {
      assert(fresh_chunks);

      if (not (*fresh_chunks).remaining())
      {
        fresh_chunks = new chunks(fresh_chunks);
      }

      return (*fresh_chunks).pop();
    }
  }

  auto deallocate(value_type * p, size_type = 1) -> void
  {
    reinterpret_cast<chunk *>(p)->next = recycled_chunk;
    recycled_chunk = reinterpret_cast<chunk *>(p);
  }
};
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_SIMPLE_ALLOCATOR_HPP
