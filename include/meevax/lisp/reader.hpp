#ifndef INCLUDED_MEEVAX_LISP_READER_HPP
#define INCLUDED_MEEVAX_LISP_READER_HPP

#include <algorithm>
#include <iterator>
#include <list>
#include <locale>
#include <string>

#include <meevax/lisp/builder.hpp>

namespace meevax::lisp
{
  struct reader
  {
    static auto tokenize(const std::string& s)
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

    auto operator()(const std::string& s) const
    {
      const auto tokens {tokenize(s)};
      const builder build {std::begin(tokens), std::end(tokens)};
      return build();
    }
  } static read {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_READER_HPP

