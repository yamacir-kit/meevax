/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_FUNCTIONAL_COMBINATOR_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_COMBINATOR_HPP

#include <meevax/functional/curry.hpp>

namespace meevax::inline functional
{
  inline auto i = [](auto&& x) constexpr
  {
    return std::forward<decltype(x)>(x);
  };

  inline auto y = [](auto&& f) constexpr -> decltype(auto)
  {
    return [&](auto&&... xs) -> decltype(auto)
    {
      return f(f, std::forward<decltype(xs)>(xs)...);
    };
  };

  inline auto z = [](auto&& f) constexpr -> decltype(auto)
  {
    return curry(std::forward<decltype(f)>(f))
                (std::forward<decltype(f)>(f));
  };
} // namespace meevax::functional

#endif // INCLUDED_MEEVAX_FUNCTIONAL_COMBINATOR_HPP
