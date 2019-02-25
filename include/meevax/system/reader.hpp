#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <iostream>
#include <iterator> // std::begin, std::end
#include <limits> // std::numeric_limits<std::streamsize>
#include <sstream>
#include <string>
#include <utility>

#include <meevax/system/cursor.hpp>
#include <meevax/system/modular.hpp>
#include <meevax/system/pair.hpp>

namespace meevax::system
{
  class reader
  {
    const cursor module;

  public:
    explicit reader(const cursor& module)
      : module {module}
    {}

    template <typename CharType>
    constexpr auto is_delimiter(CharType c) noexcept
    {
      return c == ' ' or c == '\t' or c == '\n' or c == '|' or c == '(' or c == ')' or c == '"' or c == ';';
    }

    cursor operator()(std::istream& is)
    {
      std::string buffer {};

      auto peek = [&]()
      {
        while (std::isspace(is.peek()) or is.peek() == ';')
        {
          switch (is.peek())
          {
          case ';':
            is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            break;

          default:
            is.get();
            break;
          }
        }

        return is.peek();
      };

      while (is)
      {
        switch (auto c {is.get()}; c)
        {
        case ';': // ONELINE COMMENTS
          is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
          break;

        case '(': // CONS CELLS
          if (peek() == ')') // 要素ゼロのリスト
          {
            is.get();
            return unit;
          }
          else
          {
            // std::cerr << "[debug] reader: open parentheses, invoking list constructor" << std::endl;
            return std::make_shared<pair>(is, *this);
          }

        case ')':
          // std::cerr << "[debug] reader: close parentheses, retuning unit" << std::endl;
          return unit;

        case '\n':
        case '\t':
        case ' ':
          break;

        default:
          buffer.push_back(c);

          if (is_delimiter(is.peek()))
          {
            return module.as<modular>().intern(buffer);
          }
        }
      }

      return unit;
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

