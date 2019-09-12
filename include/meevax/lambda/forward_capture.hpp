#ifndef INCLUDED_MEEVAX_LAMBDA_FORWARD_CAPTURE_HPP
#define INCLUDED_MEEVAX_LAMBDA_FORWARD_CAPTURE_HPP

#include <functional>
#include <utility>

namespace meevax::lambda
{
  template <typename T>
  using captured_type
    = typename std::conditional<
        std::is_lvalue_reference<T>::value,
        T,
        typename std::remove_reference<T>::type
      >::type;

  template <typename... Ts>
  constexpr decltype(auto) forward_capture(Ts&&... xs)
  {
    return std::tuple<captured_type<Ts>...>(std::forward<decltype(xs)>(xs)...);
  }

  template <typename T>
  constexpr decltype(auto) access(T&& x)
  {
    return std::get<0>(std::forward<decltype(x)>(x));
  }
} // namespace meevax::lambda

#endif // INCLUDED_MEEVAX_LAMBDA_FORWARD_CAPTURE_HPP

