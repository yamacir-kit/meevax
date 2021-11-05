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
    const_pointer<void> base;

    const std::size_t size;

    pointer<void> derived = nullptr;

    deallocator<void>::signature deallocate = nullptr;

  public:
    explicit region(pointer<void> const, std::size_t const);

    ~region();

    auto assigned() const noexcept -> bool;

    auto contains(std::uintptr_t const) const noexcept -> bool;

    auto contains(pointer<void> const) const noexcept -> bool;

    auto lower_bound() const noexcept -> std::uintptr_t
    {
      return reinterpret_cast<std::uintptr_t>(base);
    }

    auto release() -> void;

    auto reset(pointer<void> const, deallocator<void>::signature const) noexcept -> pointer<region>;

    auto upper_bound() const noexcept -> std::uintptr_t
    {
      return lower_bound() + size;
    }
  };
} // namespace memory
} // namespace meevax

namespace std
{
  template <>
  struct less<meevax::pointer<meevax::region>>
  {
    bool operator ()(meevax::const_pointer<meevax::region> x,
                     meevax::const_pointer<meevax::region> y) const
    {
      return (*x).upper_bound() <= (*y).lower_bound();
    }
  };
} // namespace std

#endif // INCLUDED_MEEVAX_MEMORY_REGION_HPP
