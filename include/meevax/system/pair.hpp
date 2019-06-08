#ifndef INCLUDED_MEEVAX_SYSTEM_PAIR_HPP
#define INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

#include <meevax/system/accessor.hpp>
#include <meevax/system/exception.hpp>

namespace meevax::system
{
  struct pair;

  using objective = accessor<pair>;

  extern "C" const objective unit, unbound, undefined, unspecified;

  struct pair
    : public std::pair<objective, objective>
    , public facade<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : std::pair<objective, objective> {std::forward<Ts>(args)...}
    {}
  };

  template <typename T, typename... Ts>
  constexpr decltype(auto) make(Ts&&... args)
  {
    return objective::bind<T>(std::forward<Ts>(args)...);
  }

  #define SELECTOR(NAME, INDEX) \
  template <typename Pointer> \
  decltype(auto) NAME(Pointer&& object) \
  { \
    if (object) \
    { \
      return std::get<INDEX>(object.dereference()); \
    } \
    else \
    { \
      throw error {"internal illegal selection rejected"}; \
    } \
  }

  SELECTOR(car, 0)
  SELECTOR(cdr, 1)

  std::ostream& operator<<(std::ostream&, const pair&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

