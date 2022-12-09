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

#ifndef INCLUDED_MEEVAX_MEMORY_TRACER_HPP
#define INCLUDED_MEEVAX_MEMORY_TRACER_HPP

#include <cassert>
#include <cstdint> // std::uintptr_t
#include <functional> // std::less

#include <meevax/memory/marker.hpp>
#include <meevax/utility/debug.hpp>

namespace meevax
{
inline namespace memory
{
  class tracer : public marker
  {
    void * const base;

    std::size_t const size;

    using deallocator = void (*)(void *);

    deallocator const deallocate;

  public:
    explicit tracer(void * const base)
      : base { base }
      , size { 0 }
      , deallocate { nullptr }
    {}

    template <typename T>
    explicit tracer(T * const base)
      : base { base }
      , size { sizeof(T) }
      , deallocate { [](void * base) { delete static_cast<T *>(base); } }
    {}

    ~tracer();

    auto contains(std::uintptr_t const) const noexcept -> bool;

    auto contains(void const* const) const noexcept -> bool;

    template <typename T = std::uintptr_t>
    auto lower_address() const noexcept
    {
      return reinterpret_cast<T>(base);
    }

    template <typename T = std::uintptr_t>
    auto upper_address() const noexcept
    {
      return reinterpret_cast<T>(lower_address() + size);
    }
  };

  inline auto operator <(tracer const& x, tracer const& y)
  {
    return x.upper_address() < y.lower_address();
  }
} // namespace memory
} // namespace meevax

namespace std
{
  template <>
  struct less<meevax::tracer *>
  {
    using is_transparent = void;

    auto operator ()(meevax::tracer * const x, meevax::tracer * const y) const
    {
      assert(x);
      assert(y);

      return *x < *y;
    }
  };
} // namespace std

#endif // INCLUDED_MEEVAX_MEMORY_TRACER_HPP
