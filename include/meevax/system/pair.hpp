#ifndef INCLUDED_MEEVAX_SYSTEM_PAIR_HPP
#define INCLUDED_MEEVAX_SYSTEM_PAIR_HPP

#include <meevax/system/accessor.hpp>

namespace meevax::system
{
  struct pair;

  using objective = accessor<pair>;
  using objectives = std::pair<objective, objective>;

  struct pair
    : public objectives
    , public facade<pair>
  {
    template <typename... Ts>
    constexpr pair(Ts&&... args)
      : objectives {std::forward<Ts>(args)...}
    {}
  };

  static constexpr auto* acception_message {"accessing to unit; meevax accept this (treat unit as injective) but is non-standard Scheme behavior"};

        auto& car(      objective& pair) { if (pair) { return std::get<0>(pair.access()); } else { std::cerr << warning {acception_message} << std::endl; return pair; } }
  const auto& car(const objective& pair) { if (pair) { return std::get<0>(pair.access()); } else { std::cerr << warning {acception_message} << std::endl; return pair; } }
        auto& cdr(      objective& pair) { if (pair) { return std::get<1>(pair.access()); } else { std::cerr << warning {acception_message} << std::endl; return pair; } }
  const auto& cdr(const objective& pair) { if (pair) { return std::get<1>(pair.access()); } else { std::cerr << warning {acception_message} << std::endl; return pair; } }

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

