#ifndef INCLUDED_MEEVAX_UTILITY_PERFECT_FORWARD_HPP
#define INCLUDED_MEEVAX_UTILITY_PERFECT_FORWARD_HPP

#include <utility>

#define Perfect_Forward(FROM, TO)                                              \
template <typename... Ts>                                                      \
constexpr auto FROM(Ts&&... xs)                                                \
  noexcept(noexcept(TO(std::forward<decltype(xs)>(xs)...)))                    \
  -> decltype(auto)                                                            \
{                                                                              \
  return TO(std::forward<decltype(xs)>(xs)...);                                \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#define Immutable_Perfect_Forward(FROM, TO)                                    \
template <typename... Ts>                                                      \
constexpr auto FROM(Ts&&... xs)                                                \
  const noexcept(noexcept(TO(std::forward<decltype(xs)>(xs)...)))              \
  -> decltype(auto)                                                            \
{                                                                              \
  return TO(std::forward<decltype(xs)>(xs)...);                                \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#define Static_Perfect_Forward(FROM, TO)                                       \
template <typename... Ts>                                                      \
static constexpr auto FROM(Ts&&... xs)                                         \
  noexcept(noexcept(TO(std::forward<decltype(xs)>(xs)...)))                    \
  -> decltype(auto)                                                            \
{                                                                              \
  return TO(std::forward<decltype(xs)>(xs)...);                                \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#endif // INCLUDED_MEEVAX_UTILITY_PERFECT_FORWARD_HPP

