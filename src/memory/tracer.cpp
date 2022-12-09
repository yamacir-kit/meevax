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

#include <meevax/memory/tracer.hpp>

namespace meevax
{
inline namespace memory
{
  tracer::~tracer()
  {
    if (size and deallocate)
    {
      deallocate(base);
    }
  }

  auto tracer::contains(std::uintptr_t const k) const noexcept -> bool
  {
    return lower_address() <= k and k < upper_address();
  }

  auto tracer::contains(void const* const derived) const noexcept -> bool
  {
    return contains(reinterpret_cast<std::uintptr_t>(derived));
  }
} // namespace memory
} // namespace meevax
