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
  // 最も単純なリード処理を提供する（それ以上のことは絶対にしない）
  class reader
  {
  public:
    auto operator()(const std::string& s) const
    {
      if (const auto tokens {tokenize(s)}; balance(tokens) != 0)
      {
        throw s;
      }
      else
      {
        return builder {std::begin(tokens), std::end(tokens)}();
      }
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
      const auto open {std::count(std::begin(tokens), std::end(tokens), "(")};
      const auto close {std::count(std::begin(tokens), std::end(tokens), ")")};

      return open - close;
    }
  } static read {};
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_READER_HPP

