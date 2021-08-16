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

#ifndef INCLUDED_MEEVAX_IOSTREAM_COMBINATOR_HPP
#define INCLUDED_MEEVAX_IOSTREAM_COMBINATOR_HPP

#include <iostream>

#include <meevax/type_traits/requires.hpp>

namespace meevax
{
inline namespace iostream
{
  template <typename F, typename G, REQUIRES(std::is_invocable<F, std::istream &>,
                                             std::is_invocable<G, std::istream &>)>
  auto operator |(F&& f, G&& g)
  {
    return [=](std::istream & is)
    {
      try
      {
        return f(is);
      }
      catch (...)
      {
        is.clear();
        return g(is);
      }
    };
  }

  template <typename F, typename G, REQUIRES(std::is_invocable<F, std::istream &>,
                                             std::is_invocable<G, std::istream &>)>
  auto operator +(F&& f, G&& g)
  {
    return [=](std::istream & is)
    {
      std::string s;

      s += f(is);
      s += g(is);

      return s;
    };
  }

  auto choice = [](auto&& f, auto&&... fs)
  {
    return (std::forward<decltype(f)>(f) | ... | std::forward<decltype(fs)>(fs));
  };

  auto chain = [](auto&& f, auto&&... fs)
  {
    return (std::forward<decltype(f)>(f) + ... + std::forward<decltype(fs)>(fs));
  };

  auto many = [](auto&& f)
  {
    return [=](std::istream & is)
    {
      std::string s;

      try
      {
        while (true)
        {
          s += f(is);
        }
      }
      catch (...)
      {
        return s;
      }
    };
  };
} // namespace iostream
} // namespace meevax

#endif // INCLUDED_MEEVAX_IOSTREAM_COMBINATOR_HPP
