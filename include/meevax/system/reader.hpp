#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <iostream>
#include <iterator> // std::begin, std::end
#include <limits> // std::numeric_limits<std::streamsize>
#include <list>
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
    const cursor close, dot;

  public:
    explicit reader(const cursor& module)
      : module {module}
      , close {cursor::bind<std::string>("#<close-parenthesis>")}
      , dot {cursor::bind<std::string>("#<dot>")}
    {}

    cursor operator()(std::istream& is)
    {
      for (std::string buffer {is.get()}; is; buffer.push_back(is.get())) switch (buffer.back())
      {
      case ';':
        is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        [[fallthrough]];

      case ' ': case '\t': case '\n':
        buffer.pop_back();
        break;

      case '(':
        if (auto first {(*this)(is)}; first == close) // 0
        {
          return unit;
        }
        else if (auto second {(*this)(is)}; second == close) // 1
        {
          return list(first);
        }
        else if (second == dot) // 2
        {
          return cons(first, (*this)(is));
        }
        else // 3
        {
          is.putback('(');
          return cons(first, second, (*this)(is));
        }

      case ')':
        return close;

      case '\'':
        return list(module.as<modular>().intern("quote"), (*this)(is));

      case '.':
        if (is.peek() != '.' && buffer == ".")
        {
          return dot;
        }
        [[fallthrough]];

      default:
        if (auto c {is.peek()}; std::isspace(c) or c == '|' or c == '(' or c == ')' or c == '"' or c == ';') // delimiter
        {
          return module.as<modular>().intern(buffer);
        }
      }

      return unit;
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

