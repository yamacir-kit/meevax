#ifndef INCLUDED_MEEVAX_LISP_BUILDER_HPP
#define INCLUDED_MEEVAX_LISP_BUILDER_HPP

#include <iterator>
#include <list>
#include <string>

#include <meevax/functional/fold.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/table.hpp>

namespace meevax::lisp
{
  struct builder
    : public std::list<builder>
  {
    std::string value;

    builder(const std::string& s)
      : value {s}
    {}

    // 括弧がバランスしていることが保証されていなければならない
    template <typename InputIterator>
    explicit builder(InputIterator&& begin, InputIterator&& end)
    {
      if (std::distance(begin, end) != 0)
      {
        if (*begin != "(")
        {
          if (*begin == "'")
          {
            emplace_front("quote");
            splice(std::end(*this), {++begin, end});
          }
          else value = *begin;
        }
        else while (++begin != end && *begin != ")")
        {
          emplace_back(begin, end);
        }
      }
    }

    virtual ~builder() = default;

    decltype(auto) operator()()
    {
      // expand();
      return build();
    }

  protected:
    // void expand()
    // {
    //   for (auto iter {std::begin(*this)}; iter != std::end(*this); ++iter)
    //   {
    //     if (iter->value == "'")
    //     {
    //       // クオートの次のフォームを操作中のリストの要素としてムーブする。
    //       // 残されたままのクオートはその後の操作に影響を与えない。
    //       iter->splice(std::begin(*iter), *this, std::next(iter));
    //       iter->emplace_front("quote");
    //     }
    //     else
    //     {
    //       iter->expand();
    //     }
    //   }
    // }

    cursor build() const
    {
      using namespace functional;

      return std::empty(*this)
               ? symbols.intern(std::empty(value) ? "nil" : value)
               : foldr(*this, nil, [](auto& builder, auto& constructed)
                 {
                   return builder.build() | constructed;
                 });
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_BUILDER_HPP

