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

#ifndef INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP
#define INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP

#include <meevax/memory/collector.hpp>
#include <meevax/memory/simple_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct gc_pointer
    : public simple_pointer<T>
    , private collector::interior
  {
    explicit gc_pointer(std::nullptr_t = nullptr)
    {}

    explicit gc_pointer(simple_pointer<T> const& datum)
      : simple_pointer<T> { datum }
      , collector::interior { datum.get() }
    {}

    explicit gc_pointer(gc_pointer const& datum)
      : simple_pointer<T> { datum.get() }
      , collector::interior { datum.get() }
    {}

    auto operator =(gc_pointer const& p) -> auto &
    {
      reset(p.get());
      return *this;
    }

    void reset(T * const data = nullptr)
    {
      collector::interior::reset(simple_pointer<T>::reset(data));
    }

    void swap(gc_pointer & p)
    {
      auto const copy = simple_pointer<T>::get();
      reset(p.get());
      p.reset(copy);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_GC_POINTER_HPP
