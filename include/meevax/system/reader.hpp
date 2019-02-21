#ifndef INCLUDED_MEEVAX_SYSTEM_READER_HPP
#define INCLUDED_MEEVAX_SYSTEM_READER_HPP

#include <algorithm> // std::find_if
#include <iostream>
#include <iterator> // std::begin, std::end
#include <limits>
#include <list>
#include <sstream>
#include <string>
#include <utility>

#include <meevax/system/cursor.hpp>
#include <meevax/system/modular.hpp>

namespace meevax::system
{
  class reader
  {
    const cursor module;

  public:
    explicit reader(const cursor& module)
      : module {module}
    {}

    void operator()(std::istream& is)
    {
      std::list<std::string> tokens {""};

      for (auto c {static_cast<char>(is.get())}; c != EOF; c = is.get())
      {
        switch (c)
        {
        case ';':
          is.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
          break;

        case '(':
        case ')':
          if (not tokens.back().empty())
          {
            tokens.emplace_back("");
          }
          tokens.back().push_back(c);
          tokens.emplace_back("");
          break;

        case '\n':
        case ' ':
          if (not tokens.back().empty())
          {
            tokens.emplace_back("");
          }
          break;

        case '"':
          tokens.back().push_back(c);
          tokens.back().push_back(is.get());

          while (tokens.back().back() != '"')
          {
            tokens.back().push_back(is.get());
          }
          break;

        default:
          // std::cerr << "[debug] " << tokens.back() << " -> ";
          tokens.back().push_back(c);
          // std::cerr << tokens.back() << std::endl;
        }
      }

      std::cerr << "[debug] tokens: ";
      for (const auto& each : tokens)
      {
        switch (each[0])
        {
        case '"':
          std::cerr << "\x1B[36m";
          break;

        default:
          std::cerr << "\x1B[33m";
        }

        std::cerr << each << "\x1B[0m_";
      }
      std::cerr << std::endl;
    }
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_READER_HPP

