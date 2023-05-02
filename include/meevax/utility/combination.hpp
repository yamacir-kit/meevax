/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_UTILITY_COMBINATION_HPP
#define INCLUDED_MEEVAX_UTILITY_COMBINATION_HPP

#include <tuple>
#include <utility>

namespace meevax
{
inline namespace kernel
{
  template <typename...>
  struct make_combination;

  template <typename T, auto... Is>
  struct make_combination<T, std::index_sequence<Is...>>
  {
    using type = std::tuple<std::pair<typename std::tuple_element_t<Is / std::tuple_size_v<T>, T>,
                                      typename std::tuple_element_t<Is % std::tuple_size_v<T>, T>> ...>;
  };

  template <typename... Ts>
  using combination = typename make_combination<std::tuple<Ts...>, std::make_index_sequence<sizeof...(Ts) * sizeof...(Ts)>>::type;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_UTILITY_COMBINATION_HPP
