#ifndef INCLUDED_MEEVAX_UTILITY_CURRY_HPP
#define INCLUDED_MEEVAX_UTILITY_CURRY_HPP

namespace meevax::utility
{
  template <typename F>
  constexpr decltype(auto) curry(F&& f)
  {
    return [&](auto&&... xs)
    {
      return [&](auto&&... ys)
        -> decltype(auto)
      {
        return f(std::forward<decltype(xs)>(xs)...,
                 std::forward<decltype(ys)>(ys)...);
      }
    };
  }
} // namespace meevax::utility

#endif // INCLUDED_MEEVAX_UTILITY_CURRY_HPP

