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

    template <typename... Ts>
    auto emplace(const std::string key, Ts&&... xs)
    {
      const auto result {hash_table::emplace(
        key,
        cell::make_as<T>(std::forward<Ts>(xs)...)
      )};

      return std::get<1>(std::get<0>(result));
    }
  };

  static table<symbol> symbol_table {
  };
};

#endif // INCLUDED_MEEVAX_LISP_TABLE_HPP

