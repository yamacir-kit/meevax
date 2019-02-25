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

    cursor operator()(std::istream& is)
    {
      std::string buffer {};

      while (is) switch (auto c {is.get()}; c)
      {
      case ';': // ONELINE COMMENTS
        is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        break;

      case '(': // CONS CELLS
        while (!std::isgraph(is.peek()))
        {
          is.get();
        }

        if (is.peek() == ')') // 要素ゼロのリスト
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

        switch (is.peek())
        {
        case ';':
        case '(':
        case ')':
        case '\n':
        case '\t':
        case ' ':
          return module.as<modular>().intern(buffer);

        default:
          break;
        }
      }

      return unit;
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

