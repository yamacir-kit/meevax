#ifndef INCLUDED_MEEVAX_LISP_BUILDER_HPP
#define INCLUDED_MEEVAX_LISP_BUILDER_HPP

#include <iterator>
#include <list>
#include <string>

#include <meevax/lisp/cell.hpp>
#include <meevax/lisp/function.hpp>
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

    auto operator()() const
      -> const std::shared_ptr<cell>
    {
      if (std::empty(*this))
      {
        return symbol_table.query(value);
      }
      else
      {
        auto head {cell::nil};

        for (auto iter {std::rbegin(*this)}; iter != std::rend(*this); ++iter)
        {
          head = cons((*iter)(), head);
        }

        return head;
      }
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_BUILDER_HPP

