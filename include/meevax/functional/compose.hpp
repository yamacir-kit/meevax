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

#ifndef INCLUDED_MEEVAX_FUNCTIONAL_COMPOSE_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_COMPOSE_HPP

#include <tuple>
#include <utility>

namespace meevax
{
inline namespace functional
{
  auto compose = [](auto&& f, auto&& g)
  {
    return [fs = std::forward_as_tuple(f, g)](auto&&... xs) constexpr -> decltype(auto)
    {
      return std::get<0>(fs)(std::get<1>(fs)(std::forward<decltype(xs)>(xs)...));
    };
  };
} // namespace functional
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_COMPOSE_HPP
