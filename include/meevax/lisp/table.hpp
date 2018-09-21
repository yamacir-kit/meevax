#ifndef INCLUDED_MEEVAX_LISP_TABLE_HPP
#define INCLUDED_MEEVAX_LISP_TABLE_HPP

#include <unordered_map>
#include <utility>

#include <meevax/lisp/cell.hpp>

// TODO query -> reference

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

    auto query(const std::string s)
    {
      return std::get<0>(hash_table::emplace(s, cell::make_as<T>(s)))->second;
    }
  };

  static table<symbol> symbol_table
  {
    std::make_pair("",    cell::nil),
    std::make_pair("nil", cell::nil)
  };
};

#endif // INCLUDED_MEEVAX_LISP_TABLE_HPP

