#ifndef INCLUDED_MEEVAX_FUNCTIONAL_COMBINATOR_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_COMBINATOR_HPP

#include <utility>

#include <meevax/functional/curry.hpp>

namespace meevax::functional
{
  auto y = [](auto&& lambda) constexpr
    -> decltype(auto)
  {
    return [&](auto&&... args)
      -> decltype(auto)
    {
      return lambda(lambda, std::forward<decltype(args)>(args)...);
    };
  };

  auto z = [](auto&& lambda) constexpr
    -> decltype(auto)
  {
    return curry(std::forward<decltype(lambda)>(lambda))(std::forward<decltype(lambda)>(lambda));
  };
};

#endif // INCLUDED_MEEVAX_FUNCTIONAL_COMBINATOR_HPP

