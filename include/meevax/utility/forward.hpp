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

#ifndef INCLUDED_MEEVAX_UTILITY_FORWARD_HPP
#define INCLUDED_MEEVAX_UTILITY_FORWARD_HPP

// Reference: https://vittorioromeo.info/index/blog/capturing_perfectly_forwarded_objects_in_lambdas.html

#include <utility>

namespace meevax
{
inline namespace utility
{
  #define FORWARD(...) \
    std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

  template <typename... Ts>
  constexpr decltype(auto) forward_capture(Ts&&... xs)
  {
    return std::tuple<Ts...>(FORWARD(xs)...);
  }

  #define FORWARD_CAPTURE(...) \
    forward_capture(FORWARD(__VA_ARGS__))

  template <typename... Ts>
  constexpr decltype(auto) forward_captures(Ts&&... xs)
  {
    return std::make_tuple(FORWARD_CAPTURE(xs)...);
  }

  #define FORWARD_VARIADIC_CAPTURE(...) \
    forward_captures(FORWARD(__VA_ARGS__)...)

  template <typename T>
  constexpr decltype(auto) captured(T&& x)
  {
    return std::get<0>(FORWARD(x));
  }
} // namespace utility
} // namespace meevax

#endif // INCLUDED_MEEVAX_UTILITY_FORWARD_HPP
