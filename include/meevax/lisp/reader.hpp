#ifndef INCLUDED_MEEVAX_LISP_READER_HPP
#define INCLUDED_MEEVAX_LISP_READER_HPP

#include <algorithm>
#include <iterator>
#include <list>
#include <locale>
#include <string>

#include <boost/range/algorithm.hpp>

#include <meevax/lisp/syntax_tree.hpp>

namespace meevax::lisp
{
  class reader
  {
  public:
    auto operator()(const std::string& s) const
    {
      if (const auto tokens {tokenize(s)}; balance(tokens) == 0)
      {
        const syntax_tree tree {std::begin(tokens), std::end(tokens)};
        return tree.compile(symbols);
      }
      else throw s;
    }

  protected:
    constexpr bool is_special(unsigned char c) const noexcept
    {
      return c == '\'' || c == '(' || c ==')';
    }

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
        return is_special(*iter)
                 ? std::next(iter)
                 : std::find_if(iter, std::end(s), [&](auto c)
                   {
                     return is_special(c) or std::isspace(c);
                   });
      };

      for (auto iter {find_begin(std::begin(s))}; iter != std::end(s); iter = find_begin(find_end(iter)))
      {
        tokens.emplace_back(iter, find_end(iter));
      }

      return tokens;
    }

    template <typename T>
    int balance(T&& tokens) const
    {
      return boost::count(tokens, "(") - boost::count(tokens, ")");
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_READER_HPP

