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

#ifndef INCLUDED_MEEVAX_RANGES_FOLD_LEFT_HPP
#define INCLUDED_MEEVAX_RANGES_FOLD_LEFT_HPP

#include <ranges>

namespace meevax::inline ranges
{
  template <std::ranges::input_range R>
  constexpr auto fold_left(R&& range, auto init, auto f)
  {
    for (auto&& x : range)
    {
      init = std::invoke(f, std::move(init), x);
    }

    return init;
  }
} // namespace meevax::ranges

#endif // INCLUDED_MEEVAX_RANGES_FOLD_LEFT_HPP
