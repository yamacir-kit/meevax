#ifndef INCLUDED_MEEVAX_SYSTEM_PAIR_HPP
#define INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

#include <meevax/system/accessor.hpp>

namespace meevax::system
{
  struct pair;

  using objective = accessor<pair>;

  extern "C" const objective unit;
  extern "C" const objective unbound;
  extern "C" const objective undefined;

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

  static constexpr auto* acception_message {"accessing to unit; meevax accept this (treat unit as injective) but is non-standard Scheme behavior"};

  // XXX UGLY CODE
        auto& car(      objective& pair) { if (pair) { return std::get<0>(pair.access()); } else { std::cerr << warning {acception_message} << std::endl; return pair; } }
  const auto& car(const objective& pair) { if (pair) { return std::get<0>(pair.access()); } else { std::cerr << warning {acception_message} << std::endl; return pair; } }
        auto& cdr(      objective& pair) { if (pair) { return std::get<1>(pair.access()); } else { std::cerr << warning {acception_message} << std::endl; return pair; } }
  const auto& cdr(const objective& pair) { if (pair) { return std::get<1>(pair.access()); } else { std::cerr << warning {acception_message} << std::endl; return pair; } }

  std::ostream& operator<<(std::ostream&, const pair&);
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

