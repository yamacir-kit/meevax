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

#ifndef INCLUDED_MEEVAX_FUNCTIONAL_SUBTRACTION_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_SUBTRACTION_HPP

#include <functional>
#include <ostream>

namespace meevax
{
inline namespace functional
{
  struct subtraction
  {
    template <typename T, typename... Ts>
    constexpr auto operator ()(T&& x, Ts&&... xs) const -> decltype(auto)
    {
      return (std::forward<decltype(x)>(x) - ... - std::forward<decltype(xs)>(xs));
    }

    friend auto operator <<(std::ostream & os, subtraction const&) -> std::ostream &
    {
      return os << "subtraction";
    }
  };

  constexpr subtraction subtract, sub;
} // namespace functional
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_SUBTRACTION_HPP
