#ifndef INCLUDED_MEEVAX_CORE_PAIR_HPP
#define INCLUDED_MEEVAX_CORE_PAIR_HPP

#include <iostream>
#include <utility>

#include <meevax/core/accessor.hpp>

namespace meevax::core
{
  struct pair
    : public std::pair<accessor<pair>, accessor<pair>>,
      public universal_base<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : std::pair<accessor<pair>, accessor<pair>> {std::forward<Ts>(args)...}
    {}

    // NOTE Virtual destructor is removable if instantiate this type only via std::shared_ptr.
    virtual ~pair() = default;
  };

  template <typename... Ts>
  constexpr decltype(auto) car(Ts&&... args)
  {
    return std::get<0>(std::data(std::forward<Ts>(args)...));
  }

  template <typename... Ts>
  constexpr decltype(auto) cdr(Ts&&... args)
  {
    return std::get<1>(std::data(std::forward<Ts>(args)...));
  }

  std::ostream& operator<<(std::ostream& os, const pair& exp)
  {
    os << "(" << exp.first;

    for (auto iter {exp.second}; iter; iter = cdr(iter))
    {
      if (iter.is<pair>())
      {
        os << " " << car(iter);
      }
      else // iter is the last element of dotted-list.
      {
        os << " . " << iter;
      }
    }

    return os << ")";
  }
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_PAIR_HPP

