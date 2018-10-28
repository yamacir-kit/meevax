#ifndef INCLUDED_MEEVAX_LISP_BUILDER_HPP
#define INCLUDED_MEEVAX_LISP_BUILDER_HPP

#include <iterator>
#include <list>
#include <string>

#include <meevax/functional/fold.hpp>
#include <meevax/lisp/cell.hpp>

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

    decltype(auto) operator()() const
    {
      return build();
    }

  protected:
    cursor build() const
    {
      using namespace functional;

      return std::empty(*this)
               ? std::empty(value_) ? symbols("nil") : symbols.intern(value_)
               : fold_right(*this, symbols("nil"), [](auto& build, auto& tail)
                 {
                   return build() | tail;
                 });
    }
  };
} // namespace meevax::lisp

#endif // INCLUDED_MEEVAX_LISP_BUILDER_HPP

