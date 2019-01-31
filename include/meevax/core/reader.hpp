#ifndef INCLUDED_MEEVAX_CORE_READER_HPP
#define INCLUDED_MEEVAX_CORE_READER_HPP

#include <algorithm>
#include <iterator>
#include <locale>
#include <memory>
#include <numeric>
#include <string>
#include <utility>

#include <meevax/core/syntax_tree.hpp>

#include <meevax/core/boolean.hpp>
#include <meevax/core/context.hpp>
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
      return parse(std::cbegin(tokens), std::cend(tokens)).generate();
    }

  protected:
    struct abstract_syntax_tree
      : public std::list<abstract_syntax_tree>
    {
      cursor value;

      abstract_syntax_tree(const cursor& value = nil)
        : value {value}
      {}

      cursor generate() const
      {
        if (std::empty(*this)) // is atomic value
        {
          return value;
        }
        // Following weird code performs "fold-right" algorithm.
        else return std::accumulate(std::rbegin(*this), std::rend(*this), nil, [&](auto&& rhs, auto&& lhs)
        {
          return cons(lhs.generate(), rhs);
        });
      }
    };

    template <typename InputIterator>
    auto parse(InputIterator&& iter, InputIterator&& end)
      -> abstract_syntax_tree
    {
      abstract_syntax_tree tree {};

      if (std::distance(iter, end) != 0)
      {
        if (*iter == "(") while (++iter != end && *iter != ")")
        {
          tree.emplace_back(parse(iter, end)); // TODO Able to convert constructor?
        }
        else
        {
          switch ((*iter)[0]) // スプライシングオペレータ解析
          {
          case '\'':
            tree.emplace_back(package->intern("quote"));
            tree.emplace_back(parse(++iter, end));
            break;

          case '`':
            tree.emplace_back(package->intern("quasiquote"));
            tree.emplace_back(parse(++iter, end));
            break;

          case '#':
            return expand_macro(++iter, end);

          default:
            // try
            // {
            //   return {cursor::bind<number>(*iter)};
            // }
            // catch (const std::runtime_error&)
            // {
              return {package->intern(*iter)};
            // }
          }
        }
      }

      return tree;
    }

    template <typename InputIterator>
    auto expand_macro(InputIterator&& iter, InputIterator&& end)
      -> abstract_syntax_tree
    {
      abstract_syntax_tree tree {};

      if (std::distance(iter, end) != 0)
      {
        if (*iter == "(")
        {
          tree.emplace_back(package->intern("vector"));
          tree.splice(std::next(std::begin(tree)), parse(iter, end));
        }
        else switch ((*iter)[0])
        {
        case 't':
          return {true_v};

        case 'f':
          return {false_v};

        // case 'x':
        //   return {cursor::bind<number>("0x" + std::string {std::begin(*iter) + 1, std::end(*iter)})};

        default:
          throw std::runtime_error {"unknown reader macro #" + *iter};
        }
      }

      return tree;
    }
  };

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

