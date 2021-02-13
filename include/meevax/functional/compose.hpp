#ifndef INCLUDED_MEEVAX_FUNCTIONAL_COMPOSE_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_COMPOSE_HPP

#include <tuple>
#include <utility>

namespace meevax
{
inline namespace functional
{
  auto compose = [](auto&& f, auto&& g)
  {
    return [fs = std::forward_as_tuple(f, g)](auto&&... xs) constexpr -> decltype(auto)
    {
      return std::get<0>(fs)(std::get<1>(fs)(std::forward<decltype(xs)>(xs)...));
    };
  };
} // namespace functional
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_COMPOSE_HPP
