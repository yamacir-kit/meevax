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

#ifndef INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP

#include <cstddef>
#include <meevax/memory/collector.hpp>
#include <meevax/memory/nan_boxing_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename... Ts>
  struct gc_pointer
    : public nan_boxing_pointer<Ts...>
    , private collector::traceable
  {
    explicit gc_pointer(std::nullptr_t = nullptr)
    {}

    template <typename T, REQUIRES(std::is_scalar<T>)>
    explicit gc_pointer(T const& datum)
      : nan_boxing_pointer<Ts...> { datum }
      , collector::traceable { nan_boxing_pointer<Ts...>::get() }
    {}

    explicit gc_pointer(nan_boxing_pointer<Ts...> const& datum)
      : nan_boxing_pointer<Ts...> { datum }
      , collector::traceable { nan_boxing_pointer<Ts...>::get() }
    {}

    explicit gc_pointer(gc_pointer const& gcp)
      : nan_boxing_pointer<Ts...> { gcp }
      , collector::traceable { static_cast<collector::traceable const&>(gcp) }
    {}

    auto operator =(gc_pointer const& gcp) -> auto &
    {
      reset(gcp);
      return *this;
    }

    auto reset(gc_pointer const& gcp) -> void
    {
      nan_boxing_pointer<Ts...>::reset(gcp);
      collector::traceable::reset(static_cast<collector::traceable const&>(gcp));
    }

    auto reset(std::nullptr_t = nullptr) -> void
    {
      nan_boxing_pointer<Ts...>::reset();
      collector::traceable::reset();
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP
