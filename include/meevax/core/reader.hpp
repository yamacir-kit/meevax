#ifndef INCLUDED_MEEVAX_CORE_READER_HPP
#define INCLUDED_MEEVAX_CORE_READER_HPP

#include <algorithm>
#include <iterator>
#include <list>
#include <locale>
#include <string>
#include <utility>

#include <meevax/core/syntax_tree.hpp>

namespace meevax::core
{
  auto is_graph = [](auto c) constexpr
  {
    return std::isgraph(c);
  };

  auto is_paren = [](auto c) constexpr
  {
    return c == '(' or c == ')';
  };

  auto is_macro = [](auto c) constexpr
  {
    return c == '\'' or c == '`' or c == '#';
  };

  auto is_delim = [](auto c) constexpr
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
      end = is_paren(*begin) or is_macro(*begin) ? std::next(begin) : std::find_if(begin, std::end(s), is_delim);
      tokens.emplace_back(begin, end);
    }

    return tokens;
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

