#ifndef INCLUDED_MEEVAX_LAMBDA_COMPOSE_HPP
#define INCLUDED_MEEVAX_LAMBDA_COMPOSE_HPP

#include <meevax/utility/forward.hpp>

namespace meevax::lambda
{
  auto compose = [](auto&& f, auto&& g)
  {
    return [f = FORWARD_CAPTURE(f),
            g = FORWARD_CAPTURE(g)](auto&&... operands) mutable
              -> decltype(auto)
           {
             return captured(f)(captured(g)(FORWARD(operands)...));
           };
  };
} // namespace meevax::lambda

#endif // INCLUDED_MEEVAX_LAMBDA_COMPOSE_HPP

