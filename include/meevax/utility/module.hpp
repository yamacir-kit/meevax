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

#ifndef INCLUDED_MEEVAX_UTILITY_MODULE_HPP
#define INCLUDED_MEEVAX_UTILITY_MODULE_HPP

#include <utility>

#define IMPORT(MODULE, SYMBOL, ...)                                            \
template <typename... Ts>                                                      \
auto SYMBOL(Ts&&... xs) __VA_ARGS__ -> decltype(auto)                          \
{                                                                              \
  return static_cast<MODULE __VA_ARGS__&>(*this).MODULE::SYMBOL(               \
    std::forward<decltype(xs)>(xs)...);                                        \
}                                                                              \
static_assert(true)

#define EXPORT(M, SYMBOL) using M::SYMBOL

#endif // INCLUDED_MEEVAX_UTILITY_MODULE_HPP
