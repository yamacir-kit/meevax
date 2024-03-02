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
  collector default_collector {};

  static auto reference_count = 0;

  collector::collector()
  {
    ++reference_count;
  }

  collector::~collector()
  {
    if (not --reference_count)
    {
      clear();
    }
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

  auto collector::mark() noexcept -> void
  {
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
      assert(mutator->object);

      if (not mutator->object->marked and is_root_object(mutator))
      {
        mark(mutator->object);
      }
    }
  }

  auto collector::mark(tag * const tag) noexcept -> void
  {
    assert(tag);

    if (not tag->marked)
    {
      tag->marked = true;

      const auto lower_address = reinterpret_cast<mutator *>(tag->lower_address());
      const auto upper_address = reinterpret_cast<mutator *>(tag->upper_address());

      for (auto lower = mutators.lower_bound(lower_address),
                upper = mutators.lower_bound(upper_address); lower != upper; ++lower)
      {
        mark((*lower)->object);
      }
    }
  }

  auto collector::sweep() -> void
  {
    for (auto&& tag : tags)
    {
      if (tag->marked)
      {
        tag->marked = false;
      }
      else
      {
        delete tag;
        tags.erase(tag);
      }
    }
  }
} // namespace memory
} // namespace meevax
