#ifndef INCLUDED_MEEVAX_LISP_SYNTAX_TREE_HPP
#define INCLUDED_MEEVAX_LISP_SYNTAX_TREE_HPP

#include <iterator>
#include <list>
#include <string>

#include <meevax/algorithm/fold.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/table.hpp>

namespace meevax::lisp
{
  struct syntax_tree
    : public std::list<syntax_tree>
  {
    std::string value;

    syntax_tree(const std::string& value = 0)
      : value {value}
    {}

    template <typename InputIterator>
    explicit syntax_tree(InputIterator&& begin, InputIterator&& end)
    {
      if (std::distance(begin, end) != 0)
      {
        if (*begin != "(")
        {
          if (*begin == "'")
          {
            emplace_back("quote"), emplace_back(++begin, end);
          }
          else value = *begin;
        }
        else while (++begin != end && *begin != ")")
        {
          emplace_back(begin, end);
        }
      }
    }

    template <typename Symbols>
    cursor compile(Symbols& symbols) const
    {
      if (std::empty(*this))
      {
        return std::empty(value) ? nil : intern(value, symbols);
      }
      else
      {
        return algorithm::fold_right(std::begin(*this), std::end(*this), nil, [&](auto&& car, auto&& cdr)
        {
          return car.compile(symbols) | cdr;
        });
      }
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_SYNTAX_TREE_HPP

