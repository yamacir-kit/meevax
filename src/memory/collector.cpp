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
      traceables = {};

      tracers = {};

      allocation = 0;

      threshold = 8_MiB;
    }
  }

  collector::~collector()
  {
    if (not --reference_count)
    {
      clear();

      assert(std::size(tracers) == 0);
      assert(std::size(traceables) == 0);
    }
  }

  auto collector::clear() -> void
  {
    for (auto iter = std::begin(tracers); iter != std::end(tracers); )
    {
      assert(*iter);

      if (auto * const tracer = *iter; true)
      {
        delete tracer;
        tracers.erase(iter++);
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
    return std::size(tracers);
  }

  auto collector::mark() -> void
  {
    marker::toggle();

    auto is_root = [](auto&& object)
    {
      return tracer_of(object) == std::cend(tracers);
    };

    for (auto&& object : traceables)
    {
      if (object->tracer and not object->tracer->marked() and is_root(object))
      {
        trace(object->tracer);
      }
    }
  }

  auto collector::tracer_of(void const* const p) -> decltype(collector::tracers)::iterator
  {
    tracer dummy { p, 0 };

    assert(p);

    if (auto iter = tracers.lower_bound(&dummy); iter != std::end(tracers) and (*iter)->contains(p))
    {
      return iter;
    }
    else
    {
      return std::end(tracers);
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
    for (auto iter = std::begin(tracers); iter != std::end(tracers); )
    {
      assert(*iter);

      if (auto tracer = *iter; not tracer->marked())
      {
        delete tracer;
        tracers.erase(iter++);
      }
      else
      {
        ++iter;
      }
    }
  }

  auto collector::trace(tracer * const tracer) -> void
  {
    if (tracer and not tracer->marked())
    {
      tracer->mark();

      const auto lower = traceables.lower_bound(reinterpret_cast<traceable *>(tracer->begin()));
      const auto upper = traceables.lower_bound(reinterpret_cast<traceable *>(tracer->end()));

      for (auto iter = lower; iter != upper; ++iter)
      {
        trace((*iter)->tracer);
      }
    }
  }
} // namespace memory
} // namespace meevax
