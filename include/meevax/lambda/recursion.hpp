#ifndef INCLUDED_MEEVAX_LAMBDA_RECURSION_HPP
#define INCLUDED_MEEVAX_LAMBDA_RECURSION_HPP

#include <utility>

#include <meevax/lambda/curry.hpp>

namespace meevax::lambda
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
    return curry(std::forward<decltype(lambda)>(lambda))
                (std::forward<decltype(lambda)>(lambda));
  };
} // namespace meevax::lambda

#endif // INCLUDED_MEEVAX_LAMBDA_RECURSION_HPP

