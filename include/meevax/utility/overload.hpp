#ifndef INCLUDED_MEEVAX_UTILITY_OVERLOAD_HPP
#define INCLUDED_MEEVAX_UTILITY_OVERLOAD_HPP

#include <type_traits>

namespace meevax
{
inline namespace utility
{
  template <typename... Ts>
  struct overloads
    : public Ts...
  {
    using Ts::operator()...;
  };

  template <typename... Ts>
  overloads(Ts&&...) -> overloads<Ts...>;

  template <typename... Ts>
  constexpr auto overload(Ts&&... xs) -> overloads<typename std::decay<Ts...>::type>
  {
    return { std::forward<decltype(xs)>(xs)... };
  }
} // namespace utility
} // namespace meevax

#endif // INCLUDED_MEEVAX_UTILITY_OVERLOAD_HPP

