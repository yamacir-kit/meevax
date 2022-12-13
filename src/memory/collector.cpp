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
    }
  }

  collector::~collector()
  {
    if (not --reference_count)
    {
      clear();

      assert(std::size(tracers) == 0);
      assert(std::size(registry) == 0);
    }
  }

  auto collector::clear() -> void
  {
    for (auto iter = std::begin(tracers); iter != std::end(tracers); )
    {
      assert(*iter);

      tracer_source.delete_(*iter);
      tracers.erase(iter++);
    }
  }

  auto collector::collect() -> std::size_t
  {
    auto const before = count();

    mark(), sweep();

    allocation = 0;

    return before - count();
  }

  auto collector::count() noexcept -> std::size_t
  {
    return std::size(tracers);
  }

  auto collector::mark() -> void
  {
    marker::toggle();

    auto is_root = [](auto&& registration)
    {
      return tracer_of(registration) == std::end(tracers); // If there is no tracer for the registration, it is a root object.
    };

    for (auto&& registration : registry)
    {
      assert(registration);
      assert(registration->tracer);

      if (not registration->tracer->marked() and is_root(registration))
      {
        mark(registration->tracer);
      }
    }
  }

  auto collector::mark(tracer * const tracer) -> void
  {
    assert(tracer);

    if (not tracer->marked())
    {
      tracer->mark();

      for (auto iter = registry.lower_bound(tracer->lower_address<registration *>());
           iter != std::end(registry) and *iter < tracer->upper_address<registration *>();
           ++iter)
      {
        mark((*iter)->tracer);
      }
    }
  }

  auto collector::tracer_of(void * const p) -> decltype(tracers)::iterator
  {
    assert(p);

    auto dummy = tracer(p);

    if (auto iter = tracers.lower_bound(&dummy); iter != std::end(tracers) and (*iter)->contains(p))
    {
      return iter;
    }
    else
    {
      return std::end(tracers);
    }
  }

  auto collector::sweep() -> void
  {
    for (auto iter = std::begin(tracers); iter != std::end(tracers); )
    {
      if (not (*iter)->marked())
      {
        tracer_source.delete_(*iter);
        tracers.erase(iter++);
      }
      else
      {
        ++iter;
      }
    }
  }
} // namespace memory
} // namespace meevax
