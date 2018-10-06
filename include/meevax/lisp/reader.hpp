#ifndef INCLUDED_MEEVAX_LISP_READER_HPP
#define INCLUDED_MEEVAX_LISP_READER_HPP

#include <algorithm>
#include <iostream>
#include <iterator>
#include <list>
#include <locale>
#include <string>

#include <meevax/lisp/builder.hpp>

namespace meevax::lisp
{
  class reader
  {
  public:
    // 括弧がバランスしているかのチェックを行わない点に注意
    auto operator()(const std::string& s) const
    {
      const auto tokens {tokenize(s)};
      return builder {std::begin(tokens), std::end(tokens)}();
    }

    auto operator()(std::istream& is) const
    {
      auto read_as_tokens = [&]()
      {
        std::string buffer {};
        std::getline(is, buffer);

        return tokenize(buffer);
      };

      auto tokens {read_as_tokens()};

      while (unbalance(tokens))
      {
        std::cout << ".. ";
        tokens.splice(std::end(tokens), read_as_tokens());
      }

      return builder {std::begin(tokens), std::end(tokens)}();
    }

  protected:
    auto tokenize(const std::string& s) const
      -> std::list<std::string>
    {
      std::list<std::string> tokens {};

      auto find_begin = [&](auto iter)
      {
        return std::find_if(iter, std::end(s), [](auto c)
               {
                 return std::isgraph(c);
               });
      };

      auto find_end = [&](auto iter)
      {
        return std::ispunct(*iter)
                 ? std::next(iter)
                 : std::find_if(iter, std::end(s), [&](auto c)
                   {
                     return std::ispunct(c) or std::isspace(c);
                   });
      };

      for (auto iter {find_begin(std::begin(s))}; iter != std::end(s); iter = find_begin(find_end(iter)))
      {
        tokens.emplace_back(iter, find_end(iter));
      }

      return tokens;
    }

    template <typename T>
    int unbalance(T&& tokens) const
    {
      const auto open {std::count(std::begin(tokens), std::end(tokens), "(")};
      const auto close {std::count(std::begin(tokens), std::end(tokens), ")")};

      return open - close;
    }
  } static read {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_READER_HPP

