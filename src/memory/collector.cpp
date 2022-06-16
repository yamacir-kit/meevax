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

      regions = {};

      allocation = 0;

      threshold = 8_MiB;
    }
  }

  collector::~collector()
  {
    if (not --reference_count)
    {
      clear();

      assert(std::size(regions) == 0);
      assert(std::size(objects) == 0);
    }
  }

  auto collector::clear() -> void
  {
    for (auto iter = std::begin(regions); iter != std::end(regions); )
    {
      assert(*iter);

      if (auto * const region = *iter; true)
      {
        region_allocator.delete_(region);
        iter = regions.erase(iter);
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
    return std::size(regions);
  }

  auto collector::deallocate(void * const data, std::size_t const) -> void
  {
    assert(*region_of(data));

    regions.erase(region_of(data));

    ::operator delete(data);
  }

  auto collector::mark() -> void
  {
    marker::toggle();

    for (auto&& [derived, region] : objects)
    {
      assert(region); // NOTE: objects always hold a valid region pointer.

      if (not region->marked() and region_of(derived) == std::cend(regions))
      {
        traverse(region);
      }
    }
  }

  auto collector::region_of(void const* const p) -> decltype(collector::regions)::iterator
  {
    region dummy { p, 0, };

    assert(p);

    auto not_found = std::cend(regions);

    if (auto iter = regions.lower_bound(std::addressof(dummy)); iter != not_found and (*iter)->contains(p))
    {
      return iter;
    }
    else
    {
      return not_found;
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
    for (auto iter = std::begin(regions); iter != std::end(regions); )
    {
      assert(*iter);

      if (auto region = *iter; not region->marked())
      {
        if (true)
        {
          region_allocator.delete_(region);
          iter = regions.erase(iter);
          continue;
        }
        else
        {
          region->mark();
        }
      }

      ++iter;
    }
  }

  auto collector::traverse(region * const region) -> void
  {
    if (region and not region->marked())
    {
      region->mark();

      const auto lower = objects.lower_bound(reinterpret_cast<collectable *>(region->begin()));
      const auto upper = objects.lower_bound(reinterpret_cast<collectable *>(region->end()));

      for (auto iter = lower; iter != upper; ++iter)
      {
        traverse(iter->second);
      }
    }
  }
} // namespace memory
} // namespace meevax
