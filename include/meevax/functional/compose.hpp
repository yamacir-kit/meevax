#ifndef INCLUDED_MEEVAX_FUNCTIONAL_COMPOSE_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_COMPOSE_HPP

#include <tuple>
#include <utility>

namespace meevax::functional
{
  auto compose = [](auto&& f, auto&& g)
  {
    return [f = std::forward_as_tuple(f),
            g = std::forward_as_tuple(g)]
           (auto&&... xs) mutable
              -> decltype(auto)
           {
             return
               std::get<0>(f)(
                 std::get<0>(g)(
                   std::forward<decltype(xs)>(xs)...));
           };
  };
} // namespace meevax::functional

#endif // INCLUDED_MEEVAX_FUNCTIONAL_COMPOSE_HPP

