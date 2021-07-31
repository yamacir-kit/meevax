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
    pointer<void> base, derived = nullptr;

    std::size_t size;

    deallocator<void>::signature deallocate = nullptr;

  public:
    explicit region(pointer<void> const base, std::size_t const size)
      : base { base }
      , size { size }
    {}

    ~region()
    {
      release();
    }

    auto lower_bound() const noexcept
    {
      return reinterpret_cast<std::uintptr_t>(base);
    }

    auto upper_bound() const noexcept
    {
      return lower_bound() + size;
    }

    auto contains(std::uintptr_t const k) const noexcept
    {
      return lower_bound() <= k and k < upper_bound();
    }

    auto contains(pointer<void> const derived) const noexcept
    {
      return contains(reinterpret_cast<std::uintptr_t>(derived));
    }

    auto assigned() const noexcept
    {
      return derived and deallocate;
    }

    auto reset(pointer<void> const x, deallocator<void>::signature const f) noexcept
    {
      if (not assigned())
      {
        derived = x;
        deallocate = f;
      }

      return this;
    }

    void release()
    {
      if (assigned())
      {
        deallocate(derived);
      }

      reset(nullptr, nullptr);

      size = 0;
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
