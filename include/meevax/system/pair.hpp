#ifndef INCLUDED_MEEVAX_SYSTEM_PAIR_HPP
#define INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

#include <iostream>
#include <utility>

#include <meevax/system/accessor.hpp>

namespace meevax::system
{
  struct pair
    : public std::pair<accessor<pair>, accessor<pair>>,
      public universal_base<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : std::pair<accessor<pair>, accessor<pair>> {std::forward<Ts>(args)...}
    {}

    // template <typename Reader>
    // explicit pair(std::istream& is, Reader&& read)
    // {
    //   first = read(is);
    //   is.putback('(');
    //   is.putback('.');
    //   second = read(is);
    // }

    // NOTE Virtual destructor is removable if instantiate this type only via std::shared_ptr.
    virtual ~pair() = default;
  };

  decltype(auto) car(      accessor<pair>& pair) { return std::get<0>(pair.access()); }
  decltype(auto) car(const accessor<pair>& pair) { return std::get<0>(pair.access()); }

  decltype(auto) cdr(      accessor<pair>& pair) { return std::get<1>(pair.access()); }
  decltype(auto) cdr(const accessor<pair>& pair) { return std::get<1>(pair.access()); }

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
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

