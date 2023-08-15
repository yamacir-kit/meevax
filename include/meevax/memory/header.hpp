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

#ifndef INCLUDED_MEEVAX_MEMORY_HEADER_HPP
#define INCLUDED_MEEVAX_MEMORY_HEADER_HPP

#include <cassert>
#include <cstdint> // std::uintptr_t
#include <utility> // std::forward

#include <meevax/memory/marker.hpp>

namespace meevax
{
inline namespace memory
{
  struct header
  {
    marker reacheable;

    virtual ~header() = default;

    auto contains(void const* const data) const noexcept -> bool
    {
      const auto address = reinterpret_cast<std::uintptr_t>(data);
      return lower_address() <= address and address < upper_address();
    }

    auto mark() noexcept -> void
    {
      reacheable.mark();
    }

    auto marked() const noexcept -> bool
    {
      return reacheable.marked();
    }

    virtual auto lower_address() const noexcept -> std::uintptr_t = 0;

    virtual auto upper_address() const noexcept -> std::uintptr_t = 0;
  };

  template <typename T>
  struct body : public header
  {
    T object;

    template <typename... Ts>
    explicit body(Ts&&... xs)
      : object { std::forward<decltype(xs)>(xs)... }
    {}

    ~body() override = default;

    auto lower_address() const noexcept -> std::uintptr_t override
    {
      return reinterpret_cast<std::uintptr_t>(std::addressof(object));
    }

    auto upper_address() const noexcept -> std::uintptr_t override
    {
      return lower_address() + sizeof(T);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_HEADER_HPP
