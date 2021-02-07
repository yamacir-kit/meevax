#ifndef INCLUDED_MEEVAX_PARSER_COMBINATOR_HPP
#define INCLUDED_MEEVAX_PARSER_COMBINATOR_HPP

#include <functional>
#include <iostream>

#include <meevax/parser/class.hpp>
#include <meevax/string/unicode.hpp>
#include <meevax/kernel/error.hpp> // for read_error
#include <meevax/kernel/miscellaneous.hpp> // for eof

namespace meevax
{
  template <typename T>
  using parser = std::function<T (input_port &)>;

  auto get_char = [](auto&& port = std::cin)
  {
    codeunit cu {};

    if (auto const c = port.peek(); is_eof(c))
    {
      throw read_error<eof>("exhausted input-port");
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
        throw read_error<void>("not satisfy");
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

  auto char1 = [](auto&& c)
  {
    return satisfy([=](char&& x)
    {
      return c == x;
    });
  };

  auto digit     = satisfy(is_digit);
  auto hex_digit = satisfy(is_hex_digit);
  auto upper     = satisfy(is_upper);
  auto lower     = satisfy(is_lower);
  auto letter    = satisfy(is_letter);
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PARSER_COMBINATOR_HPP
