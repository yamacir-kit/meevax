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

#ifndef INCLUDED_MEEVAX_KERNEL_STACK_HPP
#define INCLUDED_MEEVAX_KERNEL_STACK_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename T, typename... Ts>
  inline decltype(auto) push(T&& stack, Ts&&... xs)
  {
    return stack = cons(std::forward<decltype(xs)>(xs)..., stack);
  }

  template <std::size_t N, typename T>
  inline decltype(auto) pop(T&& stack)
  {
    return stack = std::next(std::begin(stack), N);
  }

  template <typename T>
  inline decltype(auto) pop(T&& stack)
  {
    let const x = car(stack);
    pop<1>(stack);
    return x;
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_STACK_HPP
