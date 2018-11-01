#ifndef INCLUDED_MEEVAX_LAMBDA_CALCULUS_COMBINATOR_HPP
#define INCLUDED_MEEVAX_LAMBDA_CALCULUS_COMBINATOR_HPP

#include <utility>

#include <meevax/lambda_calculus/curry.hpp>

namespace meevax::lambda_calculus
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
} // namespace meevax::lambda_calculus

#endif // INCLUDED_MEEVAX_LAMBDA_CALCULUS_COMBINATOR_HPP

