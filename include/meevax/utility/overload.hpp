#ifndef INCLUDED_MEEVAX_UTILITY_OVERLOAD_HPP
#define INCLUDED_MEEVAX_UTILITY_OVERLOAD_HPP

#include <type_traits>

namespace meevax
{
inline namespace utility
{
  template <typename... Ts>
  struct overloaded
    : public Ts...
  {
    using Ts::operator()...;
  };

  template <typename... Ts>
  overloaded(Ts&&...) -> overloaded<Ts...>;

  template <typename... Ts>
  constexpr auto overload(Ts&&... xs)
    -> overloaded<typename std::decay<Ts...>::type>
  {
    return { std::forward<decltype(xs)>(xs)... };
  }
} // namespace utility
} // namespace meevax

#endif // INCLUDED_MEEVAX_UTILITY_OVERLOAD_HPP

