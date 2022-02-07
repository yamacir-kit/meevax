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

#include <meevax/memory/region.hpp>

namespace meevax
{
inline namespace memory
{
  region::region(void const* const base, std::size_t const size)
    : base { base }
    , size { size }
  {}

  region::~region()
  {
    if (size)
    {
      release();
    }
  }

  auto region::assigned() const noexcept -> bool
  {
    return derived and deallocate;
  }

  auto region::contains(std::uintptr_t const k) const noexcept -> bool
  {
    return lower_bound() <= k and k < upper_bound();
  }

  auto region::contains(void const* const derived) const noexcept -> bool
  {
    return contains(reinterpret_cast<std::uintptr_t>(derived));
  }

  auto region::release() -> void
  {
    if (assigned())
    {
      deallocate(derived);
      reset();
    }
  }

  auto region::reset(void const* const x, deallocator<void>::signature const f) noexcept -> region *
  {
    derived = x;
    deallocate = f;
    return this;
  }
} // namespace memory
} // namespace meevax
