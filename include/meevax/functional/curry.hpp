#ifndef INCLUDED_MEEVAX_FUNCTIONAL_CURRY_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_CURRY_HPP

#include <utility>

namespace meevax { inline namespace functional
{
  template <typename F>
  constexpr decltype(auto) curry(F&& f)
  {
    return [&](auto&&... xs)
    {
      return [&](auto&&... ys) -> decltype(auto)
      {
        return
          f(std::forward<decltype(xs)>(xs)...,
            std::forward<decltype(ys)>(ys)...);
      };
    };
  }
}} // namespace meevax::functional

#endif // INCLUDED_MEEVAX_FUNCTIONAL_CURRY_HPP
