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

#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_UNWRAP_REFERENCE_WRAPPER_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_UNWRAP_REFERENCE_WRAPPER_HPP

#include <functional>
#include <utility>

#include <meevax/type_traits/is_reference_wrapper.hpp>

namespace meevax::inline type_traits
{
  inline auto unwrap_reference_wrapper = [](auto&& value) -> decltype(auto)
  {
    if constexpr (is_reference_wrapper<std::decay_t<decltype(value)>>::value)
    {
      return value.get();
    }
    else
    {
      return std::forward<decltype(value)>(value);
    }
  };
} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_UNWRAP_REFERENCE_WRAPPER_HPP
