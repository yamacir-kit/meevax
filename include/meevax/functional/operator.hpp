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

#ifndef INCLUDED_MEEVAX_FUNCTIONAL_OPERATOR_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_OPERATOR_HPP

#include <ostream>

#include <meevax/functional/identity.hpp>
#include <meevax/utility/overload.hpp>

namespace meevax
{
  // auto decrement = overload([](auto&& x) constexpr
  // {
  //   return --x;
  // });

  // auto                 equal_to = overload([](auto&& a, auto&& b) constexpr { return a == b; });
  // auto             not_equal_to = overload([](auto&& a, auto&& b) constexpr { return a != b; });
  // auto    less_than_or_equal_to = overload([](auto&& a, auto&& b) constexpr { return a <= b; });
  // auto    less_than             = overload([](auto&& a, auto&& b) constexpr { return a <  b; });
  // auto greater_than_or_equal_to = overload([](auto&& a, auto&& b) constexpr { return a >= b; });
  // auto greater_than             = overload([](auto&& a, auto&& b) constexpr { return a >  b; });
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_OPERATOR_HPP
