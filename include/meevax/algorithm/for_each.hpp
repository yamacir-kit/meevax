/*
   Copyright 2018-2022 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_ALGORITHM_FOR_EACH_HPP
#define INCLUDED_MEEVAX_ALGORITHM_FOR_EACH_HPP

#include <iostream>

namespace meevax
{
inline namespace algorithm
{
  template <typename C>
  struct for_each
  {
    C const& container;

    std::ostream::char_type const* seperator;

    explicit constexpr for_each(C const& container, std::ostream::char_type const* seperator = " ")
      : container { container }
      , seperator { seperator }
    {}

    auto operator ()(std::ostream & os) const -> decltype(auto)
    {
      auto const* p = "";

      for (auto const& each : container)
      {
        os << p << each;
        p = seperator;
      }

      return os;
    }
  };

  template <typename C>
  auto operator <<(std::ostream & os, for_each<C> const& print) -> decltype(auto)
  {
    return print(os);
  }
} // namespace algorithm
} // namespace meevax

#endif // INCLUDED_MEEVAX_ALGORITHM_FOR_EACH_HPP
