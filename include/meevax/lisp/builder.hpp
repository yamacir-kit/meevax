#ifndef INCLUDED_MEEVAX_LISP_BUILDER_HPP
#define INCLUDED_MEEVAX_LISP_BUILDER_HPP

#include <iterator>
#include <list>
#include <string>

#include <meevax/algorithm/fold.hpp>
#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/list.hpp>
#include <meevax/lisp/table.hpp>

namespace meevax::lisp
{
  class builder
    : public std::list<builder>
  {
    std::string value_;

  public:
    builder(const std::string& value)
      : value_ {value}
    {}

    template <typename InputIterator>
    explicit builder(InputIterator&& begin, InputIterator&& end)
    {
      if (std::distance(begin, end) != 0)
      {
        if (*begin != "(")
        {
          if (*begin == "'")
          {
            emplace_back("quote");
            emplace_back(++begin, end);
          }
          else value_ = *begin;
        }
        else while (++begin != end && *begin != ")")
        {
          emplace_back(begin, end);
        }
      }
    }

    cursor build() const
    {
      if (std::empty(*this))
      {
        return std::empty(value_) ? lookup("nil", symbols) : intern(value_, symbols);
      }
      else
      {
        return algorithm::fold_right(std::begin(*this), std::end(*this), lookup("nil", symbols), [](auto&& car, auto&& cdr)
        {
          return car.build() | cdr;
        });
      }
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_BUILDER_HPP

