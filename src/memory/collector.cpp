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

      assert(std::size(headers) == 0);
      assert(std::size(registry) == 0);
    }
  }

  auto collector::clear() -> void
  {
    for (auto&& header : headers)
    {
      delete header;
      headers.erase(header);
    }
  }

  auto collector::collect() -> void
  {
    allocation = 0;

    return mark(), sweep();
  }

  auto collector::count() noexcept -> std::size_t
  {
    return std::size(headers);
  }

  auto collector::mark() -> void
  {
    marker::toggle();

    auto is_root_object = [begin = std::begin(headers)](registration * given)
    {
      /*
         If the given `registration` is a non-root object, then an object
         containing this `registration` as a data member exists somewhere in
         memory.

         Containing the `registration` as a data member means that the address
         of the `registration` is contained in the interval of the object's
         base-address ~ base-address + object-size. The `header` is present to
         keep track of the base-address and size of the object needed here.
      */
      auto iter = headers.lower_bound(reinterpret_cast<header *>(given));

      return iter == begin or not (*--iter)->contains(given);
    };

    for (auto&& registration : registry)
    {
      assert(registration);
      assert(registration->header);

      if (not registration->header->marked() and is_root_object(registration))
      {
        mark(registration->header);
      }
    }
  }

  auto collector::mark(header * const header) -> void
  {
    assert(header);

    if (not header->marked())
    {
      header->mark();

      const auto lower_address = reinterpret_cast<registration *>(header->lower_address());
      const auto upper_address = reinterpret_cast<registration *>(header->upper_address());

      for (auto iter = registry.lower_bound(lower_address); iter != std::end(registry) and *iter < upper_address; ++iter)
      {
        mark((*iter)->header);
      }
    }
  }

  auto collector::sweep() -> void
  {
    for (auto&& header : headers)
    {
      if (not header->marked())
      {
        delete header;
        headers.erase(header);
      }
    }
  }
} // namespace memory
} // namespace meevax
