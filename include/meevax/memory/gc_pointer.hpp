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

#include <meevax/memory/collector.hpp>
#include <meevax/memory/tagged_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct gc_pointer
    : public tagged_pointer<T>
    , private collector::collectable
  {
    explicit gc_pointer(std::nullptr_t = nullptr)
    {}

    explicit gc_pointer(tagged_pointer<T> const& datum)
      : tagged_pointer<T> { datum }
      , collector::collectable { datum.get() }
    {}

    explicit gc_pointer(gc_pointer const& gcp)
      : tagged_pointer<T> { gcp.get() }
      , collector::collectable { gcp.context }
    {}

    auto operator =(gc_pointer const& gcp) -> auto &
    {
      reset(gcp);
      return *this;
    }

    auto reset(typename tagged_pointer<T>::pointer const data = nullptr) -> void
    {
      tagged_pointer<T>::reset(data);
      collector::collectable::reset(data);
    }

    auto reset(gc_pointer const& gcp) -> void
    {
      tagged_pointer<T>::reset(gcp.get());
      collector::collectable::reset(gcp.context);
    }

    // auto swap(gc_pointer & p) -> void
    // {
    //   auto const copy = tagged_pointer<T>::get();
    //   reset(p.get());
    //   p.reset(copy);
    // }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP
