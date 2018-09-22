#ifndef INCLUDED_MEEVAX_LISP_TABLE_HPP
#define INCLUDED_MEEVAX_LISP_TABLE_HPP

#include <unordered_map>
#include <utility>

#include <meevax/lisp/cell.hpp>

namespace meevax::lisp
{
  using hash_table = std::unordered_map<
                       std::string,
                       const std::shared_ptr<cell>
                     >;

  template <typename T>
  class table
    : public hash_table
  {
  public:
    template <typename... Ts>
    explicit table(Ts&&... xs)
      : hash_table {std::forward<Ts>(xs)...}
    {}

    const auto& intern(const std::string s)
    {
      emplace(s, cell::make_as<T>(s));
      return (*this)[s];
    }
  };

  static table<symbol> symbols {
    std::make_pair("", cell::nil),
    std::make_pair("nil", cell::nil)
  };
};

#endif // INCLUDED_MEEVAX_LISP_TABLE_HPP

