#ifndef INCLUDED_MEEVAX_SYSTEM_PAIR_HPP
#define INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

#include <iostream> // std::ostream
#include <utility> // std::forward

#include <meevax/system/accessor.hpp>

namespace meevax::system
{
  struct pair
    : public std::pair<accessor<pair>, accessor<pair>>
    , public facade<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : std::pair<accessor<pair>, accessor<pair>> {std::forward<Ts>(args)...}
    {}

    virtual ~pair() = default;
  };

  decltype(auto) car(      accessor<pair>& pair) { return std::get<0>(pair.access()); }
  decltype(auto) car(const accessor<pair>& pair) { return std::get<0>(pair.access()); }

  decltype(auto) cdr(      accessor<pair>& pair) { return std::get<1>(pair.access()); }
  decltype(auto) cdr(const accessor<pair>& pair) { return std::get<1>(pair.access()); }

  std::ostream& operator<<(std::ostream& os, const pair& exp)
  {
    os << "\x1b[35m(\x1b[0m" << exp.first;

    for (auto iter {exp.second}; iter; iter = cdr(iter))
    {
      if (iter.is<pair>())
      {
        os << " " << car(iter);
      }
      else // iter is the last element of dotted-list.
      {
        os << " \x1b[35m.\x1b[0m " << iter;
      }
    }

    return os << "\x1b[35m)\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

