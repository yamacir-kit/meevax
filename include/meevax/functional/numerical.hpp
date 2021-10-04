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

#ifndef INCLUDED_MEEVAX_FUNCTIONAL_NUMERICAL_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_NUMERICAL_HPP

#include <cmath>
#include <functional>
#include <ostream>

namespace meevax
{
inline namespace functional
{
  struct nanp
  {
    template <typename T>
    auto operator ()(T&& x) -> decltype(std::isnan(std::forward<decltype(x)>(x)))
    {
      return std::isnan(std::forward<decltype(x)>(x));
    }

    friend auto operator <<(std::ostream & os, nanp const&) -> std::ostream &
    {
      return os << "nan?";
    }
  };

  struct inexact_t
  {
    template <typename T>
    auto operator ()(T&& x) -> decltype(x.std::decay<T>::type::inexact())
    {
      return x.std::decay<T>::type::inexact();
    }

    friend auto operator <<(std::ostream & os, inexact_t const&) -> std::ostream &
    {
      return os << "inexact";
    }
  };
} // namespace functional
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_NUMERICAL_HPP
