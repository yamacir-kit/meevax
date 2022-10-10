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

#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <meevax/kernel/heterogeneous.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T, typename... Ts>
  auto make(Ts&&... xs)
  {
    return value_type::allocate<T>(std::forward<decltype(xs)>(xs)...); // NOTE: This leaks memory if exception thrown from T's constructor.
  }

  template <typename T>
  auto make(T&& x)
  {
    return value_type::allocate<std::decay_t<T>>(std::forward<decltype(x)>(x));
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
