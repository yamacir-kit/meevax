#ifndef INCLUDED_MEEVAX_LISP_BUILDER_HPP
#define INCLUDED_MEEVAX_LISP_BUILDER_HPP

#include <iterator>
#include <list>
#include <numeric>
#include <string>

#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/table.hpp>

namespace meevax::lisp
{
  struct builder
    : public std::list<builder>
  {
    std::string value;

    template <typename InputIterator>
    explicit builder(InputIterator&& begin, InputIterator&& end)
    {
      if (std::distance(begin, end) != 0)
      {
        if (*begin != "(")
        {
          value = *begin;
        }
        else while (++begin != end && *begin != ")")
        {
          emplace_back(begin, end);
        }
      }
    }

    virtual ~builder() = default;

    // TODO define function "foldr"
    auto operator()() const
      -> const std::shared_ptr<cell>
    {
      return std::empty(*this)
               ? symbols.intern(value)
               : std::accumulate(std::rbegin(*this), std::rend(*this), nil, [](auto init, auto elem)
                 {
                   return (elem() | init);
                 });
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_BUILDER_HPP

