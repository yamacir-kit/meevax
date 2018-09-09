#ifndef INCLUDED_MEEVAX_LISP_BUILDER_HPP
#define INCLUDED_MEEVAX_LISP_BUILDER_HPP

#include <iterator>
#include <list>
#include <string>

#include <meevax/lisp/cell.hpp>

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
        return std::empty(value) ? cell::nil : cell::make_as<std::string>(value);
      }
      else
      {
        // TODO 特殊括弧による組み込みリテラルの直接構築
        //
        // std::unordered_map<
        //   std::string,
        //   std::function<
        //     const std::shared_ptr<cell> (std::string::const_iterator begin,
        //                                  std::string::const_iterator end)
        //   >
        // > disp_table {
        //   {"vector", [](auto begin, auto end)
        //              {
        //                return cell::make_as<std::vector>(begin, end);
        //              }}
        // };

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

