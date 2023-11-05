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
  struct header : public marker
  {
    std::uintptr_t address;

    std::size_t size;

    header(void const* const address, std::size_t size)
      : address { reinterpret_cast<std::uintptr_t>(address) }
      , size { size }
    {}

    virtual ~header() = default;

    template <typename T = void>
    auto lower_address() const noexcept
    {
      return reinterpret_cast<T *>(address);
    }

    template <typename T = void>
    auto upper_address() const noexcept
    {
      return reinterpret_cast<T *>(address + size);
    }

    auto contains(void const* const data) const noexcept
    {
      return lower_address() <= data and data < upper_address();
    }
  };

  template <typename T>
  struct traceable : public header
  {
    T body;

    template <typename... Ts>
    explicit traceable(Ts&&... xs)
      : header { std::addressof(body), sizeof(T) }
      , body   { std::forward<decltype(xs)>(xs)... }
    {}

    ~traceable() override = default;
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_HEADER_HPP
