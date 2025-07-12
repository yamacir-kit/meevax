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

#ifndef INCLUDED_MEEVAX_CHRONO_DURATION_HPP
#define INCLUDED_MEEVAX_CHRONO_DURATION_HPP

#include <chrono>
#include <iostream>

namespace meevax::inline chrono
{
  using days = std::chrono::duration<std::size_t, std::ratio_multiply<std::ratio<24>, std::chrono::hours::period>>;

  using years = std::chrono::duration<std::size_t, std::ratio_multiply<std::ratio<146097, 400>, days::period>>;

  using months = std::chrono::duration<std::size_t, std::ratio_divide<years::period, std::ratio<12>>>;

  template <typename Thunk>
  auto duration(Thunk thunk)
  {
    auto begin = std::chrono::high_resolution_clock::now();
    thunk();
    return std::chrono::high_resolution_clock::now() - begin;
  }

  auto operator <<(std::ostream &, std::chrono::nanoseconds) -> std::ostream &;
} // namespace meevax::chrono

#endif // INCLUDED_MEEVAX_CHRONO_DURATION_HPP
