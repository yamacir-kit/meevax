#ifndef INCLUDED_MEEVAX_LISP_TABLE_HPP
#define INCLUDED_MEEVAX_LISP_TABLE_HPP

#include <string>
#include <unordered_map>
#include <utility>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  template <typename T>
  class table
    : public std::unordered_map<std::string, const cursor>
  {
  public:
    template <typename... Ts>
    explicit table(Ts&&... args)
      : std::unordered_map<std::string, const cursor> {std::forward<Ts>(args)...}
    {}

    const auto& intern(const std::string s)
    {
      emplace(s, make_as<T>(s));
      return (*this)[s];
    }
  };

  // Single Source of Nil
  static table<symbol> symbols {std::make_pair("nil", cursor {nullptr})};
};

#endif // INCLUDED_MEEVAX_LISP_TABLE_HPP

