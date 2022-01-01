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

#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_DELAY_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_DELAY_HPP

#include <meevax/kernel/overview.hpp> // for raise
#include <meevax/utility/demangle.hpp>

namespace meevax
{
inline namespace type_traits
{
  template <typename F>
  struct delay
  {
    static inline F f {};

    template <typename R, typename... Ts>
    static constexpr auto yield(Ts&&... xs) -> decltype(auto)
    {
      if constexpr (std::is_invocable<F, Ts...>::value)
      {
        return std::invoke(f, std::forward<decltype(xs)>(xs)...);
      }
      else if constexpr (std::is_same<R, bool>::value)
      {
        return false;
      }
      else
      {
        std::stringstream message {};
        message << "no viable operation (" << demangle(typeid(F));
        (message << ... << (" " + demangle(typeid(Ts))));
        message << ")";
        raise(message.str());
      }
    }
  };
} // namespace type_traits
} // namespace meevax

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_DELAY_HPP
