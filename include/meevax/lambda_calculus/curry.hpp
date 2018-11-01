#ifndef INCLUDED_MEEVAX_LAMBDA_CALCULUS_CURRY_HPP
#define INCLUDED_MEEVAX_LAMBDA_CALCULUS_CURRY_HPP

namespace meevax::lambda_calculus
{
  template <typename F>
  constexpr decltype(auto) curry(F&& f)
  {
    return [&](auto&&... xs)
    {
      return [&](auto&&... ys) -> decltype(auto)
      {
        return f(std::forward<decltype(xs)>(xs)...,
                 std::forward<decltype(ys)>(ys)...);
      };
    };
  }
} // namespace meevax::lambda_calculus

#endif // INCLUDED_MEEVAX_LAMBDA_CALCULUS_CURRY_HPP

