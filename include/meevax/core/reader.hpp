#ifndef INCLUDED_MEEVAX_CORE_READER_HPP
#define INCLUDED_MEEVAX_CORE_READER_HPP

#include <algorithm> // std::find_if
#include <iterator> // std::begin, std::end
#include <list>
#include <locale> // std::isgraph, std::isspace
#include <memory>
#include <numeric>
#include <string>
#include <utility>

#include <meevax/core/boolean.hpp>
#include <meevax/core/cursor.hpp>
#include <meevax/core/namescope.hpp>
#include <meevax/core/number.hpp>

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
    const std::shared_ptr<namescope> package;

  public:
    explicit reader(const std::shared_ptr<namescope>& package)
      : package {package}
    {}

    template <template <typename...> typename SequenceContainer>
    decltype(auto) operator()(const SequenceContainer<std::string>& tokens)
    {
      return operator()(std::cbegin(tokens), std::cend(tokens));
    }

  protected:
    template <typename InputIterator>
    cursor rest(InputIterator&& iter, InputIterator&& end)
    {
      if (*iter == "(")
      {
        auto buffer {operator()(iter, end)};
        return cons(buffer, rest(++iter, end));
      }
      else
      {
        auto buffer {operator()(iter, end)};
        return buffer ? cons(buffer, rest(++iter, end)) : buffer;
      }
    }

    template <typename InputIterator>
    cursor operator()(InputIterator&& iter, InputIterator&& end)
    {
      switch ((*iter)[0])
      {
      case '(': // ここで少なくともペア型であることが確定
        if (auto&& head {operator()(++iter, end)}; !head) // 先頭要素をパース
        {
          return unit; // 空リスト
        }
        else if (*++iter != ".") // トークンをひとつ先読みしてドット対かの判定
        {
          // このブロックは自分がプロパーリストを構築していることを知っている
          return cons(head, rest(iter, end));
        }
        else
        {
          // 非プロパーリストであるため単に次の式をコンスする。
          return cons(head, operator()(++iter, end));
        }

      case ')': // リスト終端もアトムであるためイテレータを進めない
        return unit;

      case '\'':
        return list(package->intern("quote"), operator()(++iter, end));

      case '`':
        return list(package->intern("quasiquote"), operator()(++iter, end));

      case '#': // reader macros
        switch ((*++iter)[0]) // TODO check next iterator is valid
        {
        case '(': // 続くリストをベクタコンストラクタにスプライシング
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

    template <typename InputIterator>
    [[deprecated]] cursor expand_macro(InputIterator&& iter, InputIterator&& end)
    {
      switch ((*iter)[0])
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
    }
  };
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_READER_HPP

