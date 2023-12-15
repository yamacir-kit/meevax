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

#if __unix__
#include <dlfcn.h> // dlopen, dlclose, dlerror
#else
#error
#endif

#include <iostream>

#include <meevax/memory/collector.hpp>
#include <meevax/memory/literal.hpp>

namespace meevax
{
inline namespace memory
{
  collector::~collector()
  {
    clear();
  }

  auto collector::clear() -> void
  {
    for (auto&& tag : tags)
    {
      delete tag;
      tags.erase(tag);
    }
  }

  auto collector::collect() -> void
  {
    allocation = 0;

    return mark(), sweep();
  }

  auto collector::count() noexcept -> std::size_t
  {
    return std::size(tags);
  }

  auto collector::dlclose(void * const handle) -> void
  {
    if (handle and ::dlclose(handle))
    {
      std::cerr << ::dlerror() << std::endl;
    }
  }

  auto collector::dlopen(std::string const& filename) -> void *
  {
    ::dlerror(); // Clear

    try
    {
      return dynamic_linked_libraries.at(filename).get();
    }
    catch (std::out_of_range const&)
    {
      if (auto handle = ::dlopen(filename.c_str(), RTLD_LAZY | RTLD_GLOBAL); handle)
      {
        dynamic_linked_libraries.emplace(std::piecewise_construct,
                                         std::forward_as_tuple(filename),
                                         std::forward_as_tuple(handle, dlclose));

        return dlopen(filename);
      }
      else
      {
        throw std::runtime_error(::dlerror());
      }
    }
  }

  auto collector::dlsym(std::string const& symbol, void * const handle) -> void *
  {
    if (auto address = ::dlsym(handle, symbol.c_str()); address)
    {
      return address;
    }
    else
    {
      throw std::runtime_error(::dlerror());
    }
  }

  auto collector::mark() -> void
  {
    marker::clear();

    auto is_root_object = [begin = tags.begin()](mutator * given)
    {
      /*
         If the given mutator is a non-root object, then an object containing
         this mutator as a data member exists somewhere in memory.

         Containing the mutator as a data member means that the address of the
         mutator is contained in the interval of the object's base-address ~
         base-address + object-size. The tag is present to keep track of the
         base-address and size of the object needed here.
      */
      auto iter = tags.lower_bound(reinterpret_cast<tag *>(given));

      return iter == begin or not (*--iter)->contains(given);
    };

    for (auto&& mutator : mutators)
    {
      assert(mutator);
      assert(mutator->location);

      if (not mutator->location->marked() and is_root_object(mutator))
      {
        mark(mutator->location);
      }
    }
  }

  auto collector::mark(tag * const tag) -> void
  {
    assert(tag);

    if (not tag->marked())
    {
      tag->mark();

      const auto lower_address = tag->lower_address<mutator>();
      const auto upper_address = tag->upper_address<mutator>();

      for (auto iter = mutators.lower_bound(lower_address); iter != mutators.end() and *iter < upper_address; ++iter)
      {
        mark((*iter)->location);
      }
    }
  }

  auto collector::sweep() -> void
  {
    for (auto&& tag : tags)
    {
      if (not tag->marked())
      {
        delete tag;
        tags.erase(tag);
      }
    }
  }

  auto primary_collector() -> collector &
  {
    static auto c = collector();
    return c;
  }
} // namespace memory
} // namespace meevax
