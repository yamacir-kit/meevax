#ifndef INCLUDED_MEEVAX_STRING_TOKENIZER_HPP
#define INCLUDED_MEEVAX_STRING_TOKENIZER_HPP

#include <iostream>
#include <list>
#include <string>
#include <limits> // std::numeric_limits<std::streamsize>

namespace meevax::string
{
  auto tokenize(std::istream& is)
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

    return tokens;
  }
} // namespace meevax::string

#endif // INCLUDED_MEEVAX_STRING_TOKENIZER_HPP

