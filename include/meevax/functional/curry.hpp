/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_FUNCTIONAL_CURRY_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_CURRY_HPP

#include <tuple>
#include <utility>

namespace meevax::inline functional
{
  template <typename F>
  constexpr auto curry(F&& f) -> decltype(auto)
  {
    return [f](auto&&... xs)
    {
      return [f, xs = std::forward_as_tuple(xs...)](auto&&... ys)
      {
        return std::apply([&](auto&&... xs)
                          {
                            return f(std::forward<decltype(xs)>(xs)...,
                                     std::forward<decltype(ys)>(ys)...);
                          }, xs);
      };
    };
  }
} // namespace meevax::functional

#endif // INCLUDED_MEEVAX_FUNCTIONAL_CURRY_HPP
