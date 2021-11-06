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

#ifndef INCLUDED_MEEVAX_MEMORY_CELL_HPP
#define INCLUDED_MEEVAX_MEMORY_CELL_HPP

#include <meevax/memory/collector.hpp>
#include <meevax/memory/simple_pointer.hpp>

namespace meevax
{
inline namespace memory
{
  template <typename T>
  struct cell
    : public simple_pointer<T>
    , private collector::object
  {
    explicit cell(std::nullptr_t = nullptr)
    {}

    explicit cell(simple_pointer<T> const& datum)
      : simple_pointer<T> { datum }
      , collector::object { datum.get() }
    {}

    explicit cell(cell const& datum)
      : simple_pointer<T> { datum.get() }
      , collector::object { datum.get() }
    {}

    auto operator =(cell const& another) -> auto &
    {
      return store(another);
    }

    void reset(T * const data = nullptr)
    {
      collector::object::reset(simple_pointer<T>::reset(data));
    }

    auto store(cell const& another) -> auto &
    {
      reset(another.get());
      return *this;
    }

    void swap(cell & another)
    {
      auto const copy = simple_pointer<T>::get();
      reset(another.get());
      another.reset(copy);
    }
  };
} // namespace memory
} // namespace meevax

#endif // INCLUDED_MEEVAX_MEMORY_CELL_HPP
