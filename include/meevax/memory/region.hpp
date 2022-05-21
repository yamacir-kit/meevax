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

#ifndef INCLUDED_MEEVAX_MEMORY_REGION_HPP
#define INCLUDED_MEEVAX_MEMORY_REGION_HPP

#include <cstdint> // std::uintptr_t
#include <functional> // std::less

#include <meevax/memory/deallocator.hpp>
#include <meevax/memory/marker.hpp>
#include <meevax/utility/debug.hpp>

namespace meevax
{
inline namespace memory
{
  class region : public marker
  {
    void const* const base;

    const std::size_t size;

    void const* derived = nullptr;

    deallocator<void>::signature deallocate = nullptr;

  public:
    explicit region(void const* const, std::size_t const);

    ~region();

    auto assigned() const noexcept -> bool;

    auto begin() const noexcept -> std::uintptr_t
    {
      return reinterpret_cast<std::uintptr_t>(base);
    }

    auto contains(std::uintptr_t const) const noexcept -> bool;

    auto contains(void const* const) const noexcept -> bool;

    auto end() const noexcept -> std::uintptr_t
    {
      return begin() + size;
    }

    auto release() -> void;

    auto reset(void const* const = nullptr, deallocator<void>::signature const = nullptr) noexcept -> region *;
  };

  inline auto operator <(region const& x, region const& y)
  {
    return x.end() < y.begin();
  }
} // namespace memory
} // namespace meevax

namespace std
{
  template <>
  struct less<meevax::region *>
  {
    auto operator ()(meevax::region * const x,
                     meevax::region * const y) const
    {
      return *x < *y;
    }
  };
} // namespace std

#endif // INCLUDED_MEEVAX_MEMORY_REGION_HPP
