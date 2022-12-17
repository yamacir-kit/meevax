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
  struct tracer : public marker
  {
    virtual ~tracer() = default;

    virtual auto lower_address() const noexcept -> std::uintptr_t = 0;

    virtual auto upper_address() const noexcept -> std::uintptr_t = 0;

    virtual auto contains(void const* const data) const noexcept -> bool = 0;
  };

  inline auto operator <(tracer const& x, tracer const& y)
  {
    return x.upper_address() < y.lower_address();
  }

  template <typename T>
  struct body : public tracer
  {
    T object;

    template <typename... Ts>
    explicit body(Ts&&... xs)
      : object { std::forward<decltype(xs)>(xs)... }
    {}

    ~body() override = default;

    auto contains(void const* const data) const noexcept -> bool override
    {
      const auto address = reinterpret_cast<std::uintptr_t>(data);
      return lower_address() <= address and address < upper_address();
    }

    auto lower_address() const noexcept -> std::uintptr_t override
    {
      return reinterpret_cast<std::uintptr_t>(std::addressof(object));
    }

    auto upper_address() const noexcept -> std::uintptr_t override
    {
      return lower_address() + sizeof(T);
    }
  };

  template <>
  struct body<void> : public tracer
  {
    void * const base;

    explicit body(void * const base)
      : base { base }
    {}

    ~body() override = default;

    auto contains(void const* const data) const noexcept -> bool override
    {
      const auto address = reinterpret_cast<std::uintptr_t>(data);
      return lower_address() <= address and address < upper_address();
    }

    auto lower_address() const noexcept -> std::uintptr_t override
    {
      return reinterpret_cast<std::uintptr_t>(base);
    }

    auto upper_address() const noexcept -> std::uintptr_t override
    {
      return lower_address();
    }
  };
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
