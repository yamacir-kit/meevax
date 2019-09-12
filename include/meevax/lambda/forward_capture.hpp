#ifndef INCLUDED_MEEVAX_LAMBDA_FORWARD_CAPTURE_HPP
#define INCLUDED_MEEVAX_LAMBDA_FORWARD_CAPTURE_HPP

#include <functional>
#include <utility>

namespace meevax::lambda
{
  #define FORWARD(...) \
    std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

  template <typename... Ts>
  constexpr decltype(auto) forward_capture(Ts&&... xs)
  {
    return std::tuple<Ts...>(FORWARD(xs)...);
  }

  #define FORWARD_CAPTURE(...) \
    forward_capture(FORWARD(__VA_ARGS__))

  template <typename... Ts>
  constexpr decltype(auto) forward_captures(Ts&&... xs)
  {
    return std::make_tuple(FORWARD_CAPTURE(xs)...);
  }

  #define FORWARD_CAPTURES(...) \
    forward_captures(FORWARD(__VA_ARGS__)...)

  template <typename T>
  constexpr decltype(auto) captured(T&& x)
  {
    return std::get<0>(FORWARD(x));
  }
} // namespace meevax::lambda

#endif // INCLUDED_MEEVAX_LAMBDA_FORWARD_CAPTURE_HPP

