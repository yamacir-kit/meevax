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
      std::string result;

      result.append(f(is));
      result.append(g(is));

      return result;
    };
  }

  template <typename F, REQUIRES(std::is_invocable<F, std::istream &>)>
  auto operator *(F&& f, std::size_t size)
  {
    return [=](std::istream & is)
    {
      std::string result;

      try
      {
        for (std::size_t i = 0; i < size; ++i)
        {
          result.append(f(is));
        }

        return result;
      }
      catch (...)
      {
        is.clear();
        return result;
      }
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

  auto many = [](auto&& f, std::size_t size = std::numeric_limits<std::size_t>::max())
  {
    return std::forward<decltype(f)>(f) * size;
  };
} // namespace iostream
} // namespace meevax

#endif // INCLUDED_MEEVAX_IOSTREAM_COMBINATOR_HPP
