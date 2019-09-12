#ifndef INCLUDED_MEEVAX_SYSTEM_PAIR_HPP
#define INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

#include <meevax/system/exception.hpp>
#include <meevax/system/object.hpp>

namespace meevax::system
{
  struct pair
    : public std::pair<object, object>
    , public facade<pair>
  {
    template <typename... Ts>
    explicit constexpr pair(Ts&&... args)
      : std::pair<object, object> {std::forward<Ts>(args)...}
    {}

    pair()
      : std::pair<object, object> {unit, unit}
    {}
  };

  #ifndef NDEBUG
  #define SELECTOR(NAME, INDEX)                                                \
  decltype(auto) NAME(const object& object)                                    \
  {                                                                            \
    if (object)                                                                \
    {                                                                          \
      return std::get<INDEX>(object.dereference());                            \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      throw error {"internal illegal selection rejected"};                     \
    }                                                                          \
  }
  #else
  #define SELECTOR(NAME, INDEX)                                                \
  decltype(auto) NAME(const object& object)                                    \
  {                                                                            \
    return std::get<INDEX>(object.dereference());                              \
  }
  #endif // NDEBUG

  SELECTOR(car, 0)
  SELECTOR(cdr, 1)

  auto operator<<(std::ostream& os, const pair& pare)
    -> decltype(os)
  {
    os << "\x1b[35m(\x1b[0m" << std::get<0>(pare);

    for (auto object {std::get<1>(pare)}; object; object = cdr(object))
    {
      if (object.is<pair>())
      {
        os << " " << car(object);
      }
      else // iter is the last element of dotted-list.
      {
        os << "\x1b[35m . \x1b[0m" << object;
      }
    }

    return os << "\x1b[35m)\x1b[0m";
  }
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

