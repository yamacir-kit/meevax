#ifndef INCLUDED_MEEVAX_CORE_PAIR_HPP
#define INCLUDED_MEEVAX_CORE_PAIR_HPP

#include <iterator>
#include <memory>
#include <string>
#include <utility>

#include <meevax/core/accessor.hpp>

namespace meevax::core
{
  // Forward decleation for cursor class.
  struct pair;

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

  struct cursor
    : public accessor<pair>,
      // TODO replace boost::iterator_facade
      public std::iterator<std::input_iterator_tag, accessor<pair>>
  {
    template <typename... Ts>
    constexpr cursor(Ts&&... args)
      : accessor<pair> {std::forward<Ts>(args)...}
    {}

    decltype(auto) operator*() const;
    // TODO operator->
    cursor& operator++();
  };

  // This class must be constructed by std::make_shared<pair>.
  struct pair
    : public std::pair<cursor, cursor>,
      public universal_base<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : std::pair<cursor, cursor> {std::forward<Ts>(args)...}
    {}

    // NOTE Virtual destructor is removable if instantiate this type only via std::shared_ptr.
    virtual ~pair() = default;

    friend std::ostream& operator<<(std::ostream& os, const pair& exp)
    {
      os << "(" << exp.first;

      for (auto iter {exp.second}; iter; ++iter)
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
  };

  decltype(auto) cursor::operator*() const
  {
    return car(*this);
  }

  cursor& cursor::operator++()
  {
    return *this = cdr(*this);
  }

  const cursor nil {nullptr};

  const auto t {cursor::bind<std::string>("true")};
  const auto f {cursor::bind<std::string>("false")};
} // namespace meevax::core

#endif // INCLUDED_MEEVAX_CORE_PAIR_HPP

