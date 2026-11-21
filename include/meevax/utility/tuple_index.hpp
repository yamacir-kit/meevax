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

#ifndef INCLUDED_MEEVAX_UTILITY_TUPLE_INDEX_HPP
#define INCLUDED_MEEVAX_UTILITY_TUPLE_INDEX_HPP

#include <cstddef>
#include <tuple>
#include <type_traits>

namespace meevax::inline utility
{
  template <typename T, typename Tuple, std::size_t I = 0>
  auto constexpr get_tuple_index() -> std::size_t
  {
    if constexpr (I == std::tuple_size_v<Tuple>)
    {
      static_assert(I < std::tuple_size_v<Tuple>);

      return I;
    }
    else if constexpr (std::is_same_v<T, std::tuple_element_t<I, Tuple>>)
    {
      return I;
    }
    else
    {
      return get_tuple_index<T, Tuple, I + 1>();
    }
  }

  template <typename T, typename Tuple>
  struct tuple_index : std::integral_constant<std::size_t, get_tuple_index<T, Tuple>()>
  {};

  template <typename T, typename Tuple>
  auto constexpr tuple_index_v = tuple_index<T, Tuple>::value;
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_TUPLE_INDEX_HPP
