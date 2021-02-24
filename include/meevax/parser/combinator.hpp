#ifndef INCLUDED_MEEVAX_PARSER_COMBINATOR_HPP
#define INCLUDED_MEEVAX_PARSER_COMBINATOR_HPP

#include <functional>
#include <iostream>
#include <type_traits>

#include <meevax/parser/class.hpp>
#include <meevax/string/unicode.hpp>
#include <meevax/kernel/error.hpp> // for read_error
#include <meevax/kernel/miscellaneous.hpp> // for eof

namespace meevax
{
  template <typename R>
  using parser = std::function<R (input_port &)>;

  auto get_char = [](auto&& port = std::cin)
  {
    codeunit cu {};

    if (auto const c = port.peek(); is_eof(c))
    {
      throw tagged_read_error<eof>("exhausted input-port");
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
        throw make<read_error>("not satisfy");
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
            REQUIRES(std::is_invocable<F, input_port &>),
            REQUIRES(std::is_invocable<G, input_port &>)>
  auto operator +(F&& f, G&& g)
  {
    return [=](input_port & port)
    {
      codeunits result {};

      result += f(port);
      result += g(port);

      return result;
    };
  }

  template <typename F, REQUIRES(std::is_invocable<F, input_port &>)>
  auto operator *(F&& f, int k)
  {
    return [=](input_port & port)
    {
      codeunits result {};

      for (auto i = 0; i < k; ++i)
      {
        result += f(port);
      }

      return result;
    };
  }

  template <typename F, REQUIRES(std::is_invocable<F, input_port &>)>
  auto operator *(int k, F&& f)
  {
    return f * k;
  }

  auto many = [](auto&& parse)
  {
    return [=](input_port & port)
    {
      codeunits result;

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

  template <typename F,
            typename G,
            REQUIRES(std::is_invocable<F, input_port &>),
            REQUIRES(std::is_invocable<G, input_port &>)>
  auto operator |(F&& f, G&& g)
  {
    return [=](input_port & port)
    {
      auto const backtrack = port.tellg();

      try
      {
        return f(port);
      }
      catch (...)
      {
        port.seekg(backtrack);
        return g(port);
      }
    };
  }

  auto backtrack = [](auto&& parse)
  {
    return [=](input_port & port)
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

  auto char1 = [](codeunit const& c)
  {
    return satisfy([=](codeunit const& x)
    {
      return c == x;
    });
  };

  auto string1 = [](codeunits const& s)
  {
    return [=](input_port & port)
    {
      for (auto const& c : s)
      {
        char1(codeunit(c, 1))(port);
      }

      return s;
    };
  };

  template <typename F,
            typename G,
            REQUIRES(std::is_invocable<F, input_port &>),
            REQUIRES(std::is_invocable<G, input_port &>)>
  auto operator <<(F&& f, G&& g)
  {
    return [=](input_port & port)
    {
      auto const result = f(port);
      g(port);
      return result;
    };
  }

  template <typename F,
            typename G,
            REQUIRES(std::is_invocable<F, input_port &>),
            REQUIRES(std::is_invocable<G, input_port &>)>
  auto operator >>(F&& f, G&& g)
  {
    return [=](input_port & port)
    {
      return f(port), g(port);
    };
  }
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PARSER_COMBINATOR_HPP
