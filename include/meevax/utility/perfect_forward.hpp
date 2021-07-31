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

#ifndef INCLUDED_MEEVAX_UTILITY_PERFECT_FORWARD_HPP
#define INCLUDED_MEEVAX_UTILITY_PERFECT_FORWARD_HPP

#include <utility>

#define Perfect_Forward(X) std::forward<decltype(X)>(X)

#define Define_Perfect_Forwarding(FROM, TO)                                    \
template <typename... Ts>                                                      \
constexpr auto FROM(Ts&&... xs)                                                \
  noexcept(noexcept(TO(Perfect_Forward(xs)...)))                               \
  -> decltype(auto)                                                            \
{                                                                              \
  return TO(Perfect_Forward(xs)...);                                           \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#define Define_Const_Perfect_Forwarding(FROM, TO)                              \
template <typename... Ts>                                                      \
constexpr auto FROM(Ts&&... xs)                                                \
  const noexcept(noexcept(TO(Perfect_Forward(xs)...)))                         \
  -> decltype(auto)                                                            \
{                                                                              \
  return TO(Perfect_Forward(xs)...);                                           \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#define Define_Static_Perfect_Forwarding(FROM, TO)                             \
template <typename... Ts>                                                      \
static constexpr auto FROM(Ts&&... xs)                                         \
  noexcept(noexcept(TO(Perfect_Forward(xs)...)))                               \
  -> decltype(auto)                                                            \
{                                                                              \
  return TO(Perfect_Forward(xs)...);                                           \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#endif // INCLUDED_MEEVAX_UTILITY_PERFECT_FORWARD_HPP

