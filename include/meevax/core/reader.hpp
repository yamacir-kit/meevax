#ifndef INCLUDED_MEEVAX_CORE_READER_HPP
#define INCLUDED_MEEVAX_CORE_READER_HPP

#include <algorithm>
#include <iterator>
#include <list>
#include <locale>
#include <string>
#include <utility>

#include <boost/range/algorithm.hpp>

#include <meevax/core/syntax_tree.hpp>

namespace meevax::core
{
  static constexpr auto is_graph = [](auto c)
  {
    return std::isgraph(c);
  };

  static constexpr auto is_paren = [](auto c)
  {
    // TODO this is weird (single quote is not parenthesis)
    return c == '(' or c == ')' or c == '\'';
  };

  // TODO 関数ディスパッチテーブルにでもしてカスタム可能にすること。
  // static constexpr auto is_macro

  static constexpr auto is_delim = [](auto c)
  {
    return is_paren(c) or std::isspace(c);
  };

  auto tokenize(const std::string& s)
  {
    std::list<std::string> tokens {};

    auto seek = [&](auto iter)
    {
      return std::find_if(iter, std::end(s), is_graph);
    };

    for (auto begin {seek(std::begin(s))}, end {begin}; begin != std::end(s); begin = seek(end))
    {
      tokens.emplace_back(
        begin, end = is_paren(*begin) ? std::next(begin) : std::find_if(begin, std::end(s), is_delim)
      );
    }

    return tokens;
  }

  template <typename T>
  int balance(T&& tokens)
  {
    return boost::count(tokens, "(") - boost::count(tokens, ")");
  }

  [[deprecated]] auto read = [](auto&& context, auto&& tokens)
  {
    // if (const auto tokens {tokenize(s)}; balance(tokens) <= 0)
    // {
      return syntax_tree {tokens}.compile(std::forward<decltype(context)>(context));
    // }
    // else throw s;
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_READER_HPP

