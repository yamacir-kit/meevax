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

#ifndef INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP

#include <meevax/memory/collector.hpp>
#include <meevax/memory/heterogeneous_pointer.hpp>
#include <meevax/memory/nan_boxing_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename... Ts>
  struct gc_pointer : public heterogeneous_pointer<nan_boxing_pointer, Ts...>
                    , private collector::mutator
  {
    using pointer = heterogeneous_pointer<nan_boxing_pointer, Ts...>;

    gc_pointer(gc_pointer const& gcp)
      : pointer { gcp }
      , collector::mutator { gcp.object }
    {}

    gc_pointer(pointer const& p)
      : pointer { p }
      , collector::mutator { locate(pointer::get()) }
    {}

    gc_pointer(std::nullptr_t = nullptr)
    {}

    template <typename T, REQUIRES(std::is_scalar<T>)>
    explicit gc_pointer(T const& datum)
      : pointer { datum }
    {
      assert(pointer::get() == nullptr);
    }

    auto operator =(gc_pointer const& gcp) -> auto &
    {
      reset(gcp);
      return *this;
    }

    auto operator =(pointer const& p) -> auto &
    {
      reset(p);
      return *this;
    }

    auto operator =(std::nullptr_t) -> auto &
    {
      reset();
      return *this;
    }

    auto reset(gc_pointer const& gcp) -> void
    {
      pointer::reset(gcp);
      collector::mutator::reset(gcp.object);
    }

    auto reset(pointer const& p) -> void
    {
      pointer::reset(p);
      collector::mutator::reset(locate(pointer::get()));
    }

    auto reset(std::nullptr_t = nullptr) -> void
    {
      pointer::reset();
      collector::mutator::reset();
    }

    template <typename T, typename Allocator, typename... Us>
    static auto make(Us&&... xs) -> gc_pointer
    {
      return pointer::template make<T, Allocator>(std::forward<decltype(xs)>(xs)...);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP
