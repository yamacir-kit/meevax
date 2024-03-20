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
  static auto reference_count = 0;

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
    }
  }

  auto collector::clear() -> void
  {
    for (auto&& object : objects)
    {
      delete object;
      objects.erase(object);
    }
  }

  auto collector::collect() -> void
  {
    allocation = 0;

    return sweep(mark());
  }

  auto collector::count() noexcept -> std::size_t
  {
    return objects.size();
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

  auto collector::mark() noexcept -> pointer_set<top>
  {
    auto is_root_object = [begin = objects.begin()](mutator const* given) // TODO INEFFICIENT!
    {
      /*
         If the given mutator is a non-root object, then an object containing
         this mutator as a data member exists somewhere in memory.

         Containing the mutator as a data member means that the address of the
         mutator is contained in the interval of the object's base-address ~
         base-address + object-size. The top is present to keep track of the
         base-address and size of the object needed here.
      */
      auto iter = objects.lower_bound(reinterpret_cast<top const*>(given));

      return iter == begin or not (*--iter)->contains(given);
    };

    auto marked_objects = pointer_set<top>();

    for (auto&& mutator : mutators)
    {
      assert(mutator);
      assert(mutator->object);

      if (not marked_objects.contains(mutator->object) and is_root_object(mutator))
      {
        mark(mutator->object, marked_objects);
      }
    }

    return marked_objects;
  }

  auto collector::mark(top const* const object, pointer_set<top> & marked_objects) noexcept -> void
  {
    assert(object);

    assert(objects.contains(object));

    if (not marked_objects.contains(object))
    {
      marked_objects.insert(object);

      auto lower = mutators.lower_bound(reinterpret_cast<mutator const*>(object->lower()));
      auto upper = mutators.lower_bound(reinterpret_cast<mutator const*>(object->upper()));

      for (; lower != upper; ++lower)
      {
        mark((*lower)->object, marked_objects);
      }
    }
  }

  auto collector::sweep(pointer_set<top> && marked_objects) -> void
  {
    for (auto marked_object : marked_objects)
    {
      objects.erase(marked_object);
    }

    for (auto object : objects)
    {
      delete object;
    }

    objects.swap(marked_objects);
  }
} // namespace memory
} // namespace meevax
