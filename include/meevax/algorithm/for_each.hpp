#ifndef INCLUDED_MEEVAX_ALGORITHM_FOR_EACH_HPP
#define INCLUDED_MEEVAX_ALGORITHM_FOR_EACH_HPP

#include <iostream>

namespace meevax
{
inline namespace algorithm
{
  template <typename C>
  struct for_each
  {
    C const& container;

    std::ostream::char_type const* seperator;

    explicit constexpr for_each(C const& container, std::ostream::char_type const* seperator = " ")
      : container { container }
      , seperator { seperator }
    {}

    auto operator ()(std::ostream & os) const -> decltype(auto)
    {
      auto const* p = "";

      for (auto const& each : container)
      {
        os << p << each;
        p = seperator;
      }

      return os;
    }
  };

  template <typename C>
  auto operator <<(std::ostream & os, for_each<C> const& print) -> decltype(auto)
  {
    return print(os);
  }
} // namespace algorithm
} // namespace meevax

#endif // INCLUDED_MEEVAX_ALGORITHM_FOR_EACH_HPP
