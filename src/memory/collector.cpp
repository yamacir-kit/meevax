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

#include <meevax/memory/collector.hpp>

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

      // threshold = std::numeric_limits<std::size_t>::max();
      threshold = 8_MiB;
    }
  }

  collector::~collector()
  {
    if (not --reference_count)
    {
      /* ---- NOTE -------------------------------------------------------------
       *
       *  We're using collect instead of clear to check that all objects can be
       *  collected. If speed is a priority, clear should be used here.
       *
       * -------------------------------------------------------------------- */

      collect();
      collect(); // XXX: vector elements

      assert(std::size(objects) == 0);
      assert(std::size(regions) == 0);
    }
  }

  auto collector::allocate(std::size_t const size) -> pointer<void>
  {
    if (auto data = ::operator new(size); data)
    {
      if (overflow())
      {
        collect();
      }

      allocation += size;

      regions.insert(new region(data, size));

      return data;
    }
    else
    {
      throw std::bad_alloc();
    }
  }

  void collector::clear()
  {
    for (auto iter = std::begin(regions); iter != std::end(regions); )
    {
      assert(*iter);

      if (pointer<region> region = *iter; region->assigned())
      {
        delete region;
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

  auto collector::count() const noexcept -> std::size_t
  {
    return std::size(regions);
  }

  void collector::deallocate(pointer<void> const data, std::size_t const)
  {
    try
    {
      if (auto const iter = region_of(data); *iter)
      {
        regions.erase(iter);
      }
    }
    catch (...)
    {}

    ::operator delete(data);
  }

  void collector::mark()
  {
    marker::toggle();

    for (auto [derived, region] : objects)
    {
      if (region and not region->marked() and region_of(derived) == std::cend(regions))
      {
        traverse(region);
      }
    }
  }

  bool collector::overflow() const noexcept
  {
    return threshold < allocation;
  }

  auto collector::region_of(pointer<void> const interior) -> decltype(collector::regions)::iterator
  {
    region dummy { interior, 0 };

    if (auto iter = regions.lower_bound(&dummy); iter != std::cend(regions) and (**iter).contains(interior))
    {
      return iter;
    }
    else
    {
      return std::cend(regions);
    }
  }

  auto collector::reset(pointer<void> const derived, deallocator<void>::signature const deallocate) -> pointer<region>
  {
    if (auto const lock = std::unique_lock(resource); lock and derived)
    {
      auto const iter = region_of(derived);

      assert(iter != std::cend(regions));
      assert(deallocate);

      return (*iter)->reset(derived, deallocate);
    }
    else
    {
      return nullptr;
    }
  }

  void collector::reset_threshold(std::size_t const size)
  {
    if (auto const lock = std::unique_lock(resource); lock)
    {
      threshold = size;
    }
  }

  void collector::sweep()
  {
    for (auto iter = std::begin(regions); iter != std::end(regions); )
    {
      assert(*iter);

      if (pointer<region> region = *iter; not region->marked())
      {
        if (region->assigned())
        {
          delete region;
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

  void collector::traverse(pointer<region> const the_region)
  {
    if (the_region and not the_region->marked())
    {
      the_region->mark();

      auto lower = objects.lower_bound(reinterpret_cast<pointer<object>>(the_region->lower_bound()));
      auto upper = objects.lower_bound(reinterpret_cast<pointer<object>>(the_region->upper_bound()));

      for (auto iter = lower; iter != upper; ++iter)
      {
        traverse(iter->second);
      }
    }
  }
} // namespace memory
} // namespace meevax

auto operator new(std::size_t const size, meevax::collector & gc) -> meevax::pointer<void>
{
  return gc.allocate(size);
}

void operator delete(meevax::pointer<void> const data, meevax::collector & gc) noexcept
{
  gc.deallocate(data);
}
