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

#ifndef INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP
#define INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP

#include <meevax/functional/identity.hpp>
#include <meevax/memory/cell.hpp>
#include <meevax/string/append.hpp>
#include <meevax/type_traits/requires.hpp>

#define NIL /* nothing */

namespace meevax
{
inline namespace kernel
{
  struct error;          // error.hpp
  struct exact_integer;  // exact_integer.hpp
  struct pair;           // pair.hpp
  struct ratio;          // ratio.hpp

  template <typename T>
  struct floating_point; // floating_point.hpp

  using single_float = floating_point<float>;
  using double_float = floating_point<double>; // NOTE: 0.0 is double

  template <template <typename...> typename Pointer, typename T>
  class heterogeneous;

  using                         object = heterogeneous<cell, pair>;
  using                   let = object;
  using       reference = let      &;
  using const_reference = let const&;

  using null = std::nullptr_t;

  template <typename... Ts>
  using define [[deprecated]] = typename identity<Ts...>::type;

  [[noreturn]]
  auto raise(std::string const&) -> void; // error.hpp
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP
