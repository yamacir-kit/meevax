#ifndef INCLUDED_MEEVAX_CORE_PAIR_HPP
#define INCLUDED_MEEVAX_CORE_PAIR_HPP

#include <iterator>
#include <memory>
#include <string>
#include <utility>

#include <boost/iterator/iterator_facade.hpp>

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

  struct cursor
    : public accessor<pair>,
      public std::iterator<std::input_iterator_tag, cursor>
  {
    template <typename... Ts>
    constexpr cursor(Ts&&... args)
      : accessor<pair> {std::forward<Ts>(args)...}
    {}

    decltype(auto) operator*() const
    {
      return std::data(*this).first;
    }

    decltype(auto) operator->() const
    {
      return std::data(*this).first;
    }

    decltype(auto) operator++()
    {
      return *this = std::data(*this).second;
    }
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

    for (cursor iter {exp.second}; iter; ++iter)
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

  const cursor unit {nullptr};
  const cursor undefined {nullptr};
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_PAIR_HPP

