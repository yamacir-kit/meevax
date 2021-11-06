/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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
    const_void_pointer base;

    const std::size_t size;

    void_pointer derived = nullptr;

    deallocator<void>::signature deallocate = nullptr;

  public:
    explicit region(const_void_pointer, std::size_t const);

    ~region();

    auto assigned() const noexcept -> bool;

    auto contains(std::uintptr_t const) const noexcept -> bool;

    auto contains(const_void_pointer) const noexcept -> bool;

    auto lower_bound() const noexcept -> std::uintptr_t
    {
      return reinterpret_cast<std::uintptr_t>(base);
    }

    auto release() -> void;

    auto reset(const_void_pointer, deallocator<void>::signature const) noexcept -> pointer_to<region>;

    auto upper_bound() const noexcept -> std::uintptr_t
    {
      return lower_bound() + size;
    }
  };

  inline auto operator <(region const& x, region const& y)
  {
    return x.upper_bound() < y.lower_bound();
  }
} // namespace memory
} // namespace meevax

namespace std
{
  template <>
  struct less<meevax::pointer_to<meevax::region>>
  {
    auto operator ()(meevax::const_pointer_to<meevax::region> x,
                     meevax::const_pointer_to<meevax::region> y) const
    {
      return *x < *y;
    }
  };
} // namespace std

#endif // INCLUDED_MEEVAX_MEMORY_REGION_HPP
