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
  auto decrement = overload([](auto&& x) constexpr
  {
    return --x;
  });

  #define DEFINE_OPERATOR(NAME, BASE)                                          \
  struct NAME : public BASE                                                    \
  {                                                                            \
    friend auto operator <<(std::ostream & os, NAME const&) -> std::ostream &  \
    {                                                                          \
      return os << #NAME;                                                      \
    }                                                                          \
  }

  DEFINE_OPERATOR(add, std::plus<void>);

  #undef DEFINE_OPERATOR

  // auto add                      = overload([](auto&& a, auto&& b) constexpr { return a +  b; });
  // auto multiply                 = overload([](auto&& a, auto&& b) constexpr { return a *  b; });
  // auto subtract                 = overload([](auto&& a, auto&& b) constexpr { return a -  b; });
  // auto divide                   = overload([](auto&& a, auto&& b) constexpr { return a /  b; });
  // auto modulo                   = overload([](auto&& a, auto&& b) constexpr { return a %  b; });
  //
  // auto                 equal_to = overload([](auto&& a, auto&& b) constexpr { return a == b; });
  // auto             not_equal_to = overload([](auto&& a, auto&& b) constexpr { return a != b; });
  // auto    less_than_or_equal_to = overload([](auto&& a, auto&& b) constexpr { return a <= b; });
  // auto    less_than             = overload([](auto&& a, auto&& b) constexpr { return a <  b; });
  // auto greater_than_or_equal_to = overload([](auto&& a, auto&& b) constexpr { return a >= b; });
  // auto greater_than             = overload([](auto&& a, auto&& b) constexpr { return a >  b; });
  //
  // auto operator <<(std::ostream & port, decltype(add                     ) const&) -> std::ostream & { return port << "add"; }
  // auto operator <<(std::ostream & port, decltype(multiply                ) const&) -> std::ostream & { return port << "multiply"; }
  // auto operator <<(std::ostream & port, decltype(subtract                ) const&) -> std::ostream & { return port << "subtract"; }
  // auto operator <<(std::ostream & port, decltype(divide                  ) const&) -> std::ostream & { return port << "divide"; }
  // auto operator <<(std::ostream & port, decltype(modulo                  ) const&) -> std::ostream & { return port << "modulo"; }
  //
  // auto operator <<(std::ostream & port, decltype(                equal_to) const&) -> std::ostream & { return port << "equal_to"; }
  // auto operator <<(std::ostream & port, decltype(            not_equal_to) const&) -> std::ostream & { return port << "not_equal_to"; }
  // auto operator <<(std::ostream & port, decltype(   less_than_or_equal_to) const&) -> std::ostream & { return port << "less_than_or_equal_to"; }
  // auto operator <<(std::ostream & port, decltype(   less_than            ) const&) -> std::ostream & { return port << "less_than"; }
  // auto operator <<(std::ostream & port, decltype(greater_than_or_equal_to) const&) -> std::ostream & { return port << "greater_than_or_equal_to"; }
  // auto operator <<(std::ostream & port, decltype(greater_than            ) const&) -> std::ostream & { return port << "greater_than"; }
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_OPERATOR_HPP
