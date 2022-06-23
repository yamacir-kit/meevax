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

#include <meevax/memory/collector.hpp>
#include <meevax/memory/literal.hpp>

namespace meevax
{
inline namespace memory
{
  static std::size_t reference_count = 0;

  collector::collector()
  {
    if (not reference_count++)
    {
      objects = {};

      headers = {};

      allocation = 0;

      threshold = 8_MiB;
    }
  }

  collector::~collector()
  {
    if (not --reference_count)
    {
      clear();

      assert(std::size(headers) == 0);
      assert(std::size(objects) == 0);
    }
  }

  auto collector::clear() -> void
  {
    for (auto iter = std::begin(headers); iter != std::end(headers); )
    {
      assert(*iter);

      if (auto * const header = *iter; true)
      {
        header_allocator.delete_(header);
        iter = headers.erase(iter);
      }
      else
      {
        ++iter;
      }
    }
  }

  auto collector::collect() -> std::size_t
  {
    auto const before = count();

    if (auto const lock = std::unique_lock(resource); lock)
    {
      mark(), sweep();

      allocation = 0;
    }

    return before - count();
  }

  auto collector::count() noexcept -> std::size_t
  {
    return std::size(headers);
  }

  auto collector::mark() -> void
  {
    marker::toggle();

    auto is_root = [](auto&& object)
    {
      return header_of(object) == std::cend(headers);
    };

    for (auto&& object : objects)
    {
      if (object->header and not object->header->marked() and is_root(object))
      {
        trace(object->header);
      }
    }
  }

  auto collector::header_of(void const* const p) -> decltype(collector::headers)::iterator
  {
    header dummy { p, 0, };

    assert(p);

    if (auto iter = headers.lower_bound(&dummy); iter != std::end(headers) and (*iter)->contains(p))
    {
      return iter;
    }
    else
    {
      return std::end(headers);
    }
  }

  auto collector::reset_threshold(std::size_t const size) -> void
  {
    if (auto const lock = std::unique_lock(resource); lock)
    {
      threshold = size;
    }
  }

  auto collector::sweep() -> void
  {
    for (auto iter = std::begin(headers); iter != std::end(headers); )
    {
      assert(*iter);

      if (auto header = *iter; not header->marked())
      {
        header_allocator.delete_(header);
        iter = headers.erase(iter);
      }
      else
      {
        ++iter;
      }
    }
  }

  auto collector::trace(header * const header) -> void
  {
    if (header and not header->marked())
    {
      header->mark();

      const auto lower = objects.lower_bound(reinterpret_cast<collectable *>(header->begin()));
      const auto upper = objects.lower_bound(reinterpret_cast<collectable *>(header->end()));

      for (auto iter = lower; iter != upper; ++iter)
      {
        trace((*iter)->header);
      }
    }
  }
} // namespace memory
} // namespace meevax
