#ifndef INCLUDED_MEEVAX_CORE_READER_HPP
#define INCLUDED_MEEVAX_CORE_READER_HPP

#include <algorithm>
#include <iterator>
#include <list>
#include <locale>
#include <memory>
#include <numeric>
#include <string>
#include <utility>

#include <meevax/core/syntax_tree.hpp>

#include <meevax/core/boolean.hpp>
#include <meevax/core/context.hpp>
#include <meevax/core/number.hpp>
#include <meevax/core/pair.hpp>

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

  template <template <typename...> typename SequenceContainer>
  auto tokenize(const std::string& s)
  {
    SequenceContainer<std::string> tokens {};

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

  class reader
  {
    const std::shared_ptr<context> package;

  public:
    explicit reader(const std::shared_ptr<context>& package)
      : package {package}
    {}

    template <template <typename...> typename SequenceContainer>
    decltype(auto) operator()(const SequenceContainer<std::string>& tokens)
    {
      return operator()(std::cbegin(tokens), std::cend(tokens));
    }

  protected:
    template <typename InputIterator>
    cursor list_(InputIterator&& iter, InputIterator&& end)
    {
      if (*iter == "(")
      {
        auto buffer {operator()(iter, end)};
        return cons(buffer, list_(++iter, end));
      }
      else
      {
        auto buffer {operator()(iter, end)};
        return buffer ? cons(buffer, list_(++iter, end)) : buffer;
      }
    }

    template <typename InputIterator>
    cursor operator()(InputIterator&& iter, InputIterator&& end)
    {
      switch (auto token {*iter}; token[0])
      {
      case '(':
        if (auto&& car {operator()(++iter, end)}; !car)
        {
          return nil;
        }
        else if (*++iter != ".")
        {
          return cons(car, list_(iter, end));
        }
        else
        {
          return cons(car, operator()(++iter, end));
        }

      case ')':
        return nil;

      case '\'':
        return list(package->intern("quote"), operator()(++iter, end));

      case '`':
        return list(package->intern("quasiquote"), operator()(++iter, end));

      case '#':
        switch ((*++iter)[0]) // TODO check next iterator is valid
        {
        case '(':
          return cons(package->intern("vector"), operator()(iter, end));

        case 't':
          return true_v;

        case 'f':
          return false_v;

        // case 'x':
        //   return {cursor::bind<number>("0x" + std::string {std::begin(*iter) + 1, std::end(*iter)})};

        default:
          throw std::runtime_error {"unknown reader macro #" + *iter};
        }

      default:
        try // XXX Dirty hack!!!
        {
          return {cursor::bind<number>(*iter)};
        }
        catch (const std::runtime_error&) // is not number
        {
          return package->intern(*iter);
        }
      }
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_READER_HPP

