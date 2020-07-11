#ifndef INCLUDED_MEEVAX_FUNCTIONAL_COMBINATOR_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_COMBINATOR_HPP

#include <meevax/functional/curry.hpp>

namespace meevax { inline namespace functional
{
  auto i = [](auto&& x) constexpr
  {
    return std::forward<decltype(x)>(x);
  };

  auto y = [](auto&& f) constexpr
    -> decltype(auto)
  {
    return [&](auto&&... xs)
      -> decltype(auto)
    {
      return f(f, std::forward<decltype(xs)>(xs)...);
    };
  };

  auto z = [](auto&& f) constexpr
    -> decltype(auto)
  {
    return
      curry(std::forward<decltype(f)>(f))
           (std::forward<decltype(f)>(f));
  };
}} // namespace meevax::functional

#endif // INCLUDED_MEEVAX_FUNCTIONAL_COMBINATOR_HPP
