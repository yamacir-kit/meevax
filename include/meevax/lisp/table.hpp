#ifndef INCLUDED_MEEVAX_LISP_TABLE_HPP
#define INCLUDED_MEEVAX_LISP_TABLE_HPP

#include <unordered_map>
#include <utility>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  template <typename T>
  class table
    : public std::unordered_map<std::string, cursor>
  {
  public:
    template <typename... Ts>
    explicit table(Ts&&... xs)
      : std::unordered_map<std::string, cursor> {std::forward<Ts>(xs)...}
    {}

    const auto& intern(const std::string s)
    {
      emplace(s, make_as<T>(s));
      return (*this)[s];
    }
  };

  static table<symbol> symbols {std::make_pair("nil", nil)};
};

#endif // INCLUDED_MEEVAX_LISP_TABLE_HPP

