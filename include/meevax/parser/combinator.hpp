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

#ifndef INCLUDED_MEEVAX_PARSER_COMBINATOR_HPP
#define INCLUDED_MEEVAX_PARSER_COMBINATOR_HPP

#include <functional>
#include <iostream>
#include <type_traits>

#include <meevax/parser/class.hpp>
#include <meevax/kernel/error.hpp> // for read_error
#include <meevax/kernel/miscellaneous.hpp> // for eof

namespace meevax
{
  template <typename R>
  using parser = std::function<R (std::istream &)>;

  auto get_char = [](auto&& port = std::cin)
  {
    std::string cu {};

    if (auto const c = port.peek(); is_eof(c))
    {
      throw eof();
    }
    else if (0b1111'0000 < c)
    {
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
    }
    else if (0b1110'0000 < c)
    {
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
    }
    else if (0b1100'0000 < c)
    {
      cu.push_back(port.narrow(port.get(), '\0'));
      cu.push_back(port.narrow(port.get(), '\0'));
    }
    else
    {
      cu.push_back(port.narrow(port.get(), '\0'));
    }

    return cu;
  };

  auto satisfy = [](auto&& check)
  {
    return [=](auto&& port)
    {
      auto const g = port.tellg();

      if (auto const c = get_char(port); check(c))
      {
        port.seekg(g);
        throw read_error("not satisfy");
      }
      else
      {
        return c;
      }
    };
  };

  auto any = satisfy([](auto&&...)
  {
    return true;
  });

  template <typename F,
            typename G,
            REQUIRES(std::is_invocable<F, std::istream &>),
            REQUIRES(std::is_invocable<G, std::istream &>)>
  auto operator +(F&& f, G&& g)
  {
    return [=](std::istream & port)
    {
      std::string result {};

      result += f(port);
      result += g(port);

      return result;
    };
  }

  template <typename F, REQUIRES(std::is_invocable<F, std::istream &>)>
  auto operator *(F&& f, int k)
  {
    return [=](std::istream & port)
    {
      std::string result {};

      for (auto i = 0; i < k; ++i)
      {
        result += f(port);
      }

      return result;
    };
  }

  template <typename F, REQUIRES(std::is_invocable<F, std::istream &>)>
  auto operator *(int k, F&& f)
  {
    return f * k;
  }

  auto many = [](auto&& parse)
  {
    return [=](std::istream & port)
    {
      std::string result;

      try
      {
        while (true)
        {
          result += parse(port);
        }
      }
      catch (...)
      {}

      return unit;
    };
  };

  // template <typename F,
  //           typename G,
  //           REQUIRES(std::is_invocable<F, std::istream &>),
  //           REQUIRES(std::is_invocable<G, std::istream &>)>
  // auto operator |(F&& f, G&& g)
  // {
  //   return [=](std::istream & port)
  //   {
  //     auto const backtrack = port.tellg();
  //
  //     try
  //     {
  //       return f(port);
  //     }
  //     catch (...)
  //     {
  //       port.seekg(backtrack);
  //       return g(port);
  //     }
  //   };
  // }

  auto backtrack = [](auto&& parse)
  {
    return [=](std::istream & port)
    {
      auto const g = port.tellg();

      try
      {
        return parse(port);
      }
      catch (...)
      {
        port.seekg(g);
        throw;
      }
    };
  };

  auto char1 = [](std::string const& c)
  {
    return satisfy([=](std::string const& x)
    {
      return c == x;
    });
  };

  auto string1 = [](std::string const& s)
  {
    return [=](std::istream & port)
    {
      for (auto const& c : s)
      {
        char1(std::string(c, 1))(port);
      }

      return s;
    };
  };

  template <typename F,
            typename G,
            REQUIRES(std::is_invocable<F, std::istream &>),
            REQUIRES(std::is_invocable<G, std::istream &>)>
  auto operator <<(F&& f, G&& g)
  {
    return [=](std::istream & port)
    {
      auto const result = f(port);
      g(port);
      return result;
    };
  }

  template <typename F,
            typename G,
            REQUIRES(std::is_invocable<F, std::istream &>),
            REQUIRES(std::is_invocable<G, std::istream &>)>
  auto operator >>(F&& f, G&& g)
  {
    return [=](std::istream & port)
    {
      return f(port), g(port);
    };
  }
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PARSER_COMBINATOR_HPP
