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

#ifndef INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
#define INCLUDED_MEEVAX_KERNEL_PREFACE_HPP

#include <meevax/functional/identity.hpp>
#include <meevax/memory/cell.hpp>
#include <meevax/posix/vt10x.hpp>
#include <meevax/string/append.hpp>
#include <meevax/type_traits/requires.hpp>
#include <meevax/utility/hexdump.hpp>

#define NIL /* nothing */

namespace meevax
{
inline namespace kernel
{
  struct exact_integer;
  struct pair;
  struct ratio;

  template <typename T>
  struct floating_point;

  using single_float = floating_point<float>;
  using double_float = floating_point<double>;
  using system_float = floating_point<decltype(0.0)>;

  template <template <typename...> typename Pointer, typename T>
  class heterogeneous;

  using let = heterogeneous<cell, pair>;

  using null = std::nullptr_t;

  template <typename... Ts>
  using define [[deprecated]] = typename identity<Ts...>::type;

  template <typename... Ts>
  auto make_error(Ts&&... xs)
  {
    return std::runtime_error(string_append(std::forward<decltype(xs)>(xs)...));
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PREFACE_HPP
