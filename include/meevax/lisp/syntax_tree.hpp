#ifndef INCLUDED_MEEVAX_LISP_SYNTAX_TREE_HPP
#define INCLUDED_MEEVAX_LISP_SYNTAX_TREE_HPP

#include <iterator>
#include <list>
#include <string>

#include <meevax/algorithm/fold.hpp>
#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  struct syntax_tree
    : public std::list<syntax_tree>
  {
    const std::string value;

    syntax_tree(const std::string& value = 0)
      : value {value}
    {}

    template <template <typename...> typename SequenceContainer>
    explicit syntax_tree(const SequenceContainer<std::string>& tokens)
      : syntax_tree {std::begin(tokens), std::end(tokens)}
    {}

    template <typename InputIterator>
    explicit syntax_tree(InputIterator&& begin, InputIterator&& end)
      : value {std::distance(begin, end) ? *begin : ""}
    {
      if (value != "(")
      {
        if (value == "'")
        {
          emplace_back("quote"), emplace_back(++begin, end);
        }
      }
      else while (++begin != end && *begin != ")")
      {
        emplace_back(begin, end);
      }
    }

    template <typename Context>
    cursor compile(Context&& context) const
    {
      if (std::empty(*this))
      {
        return std::empty(value) || value == "(" ? nil : context.intern(value);
      }
      else return algorithm::fold_right(std::begin(*this), std::end(*this), nil, [&](auto&& car, auto&& cdr)
      {
        return car.compile(context) | cdr;
      });
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_SYNTAX_TREE_HPP

