#ifndef INCLUDED_MEEVAX_UTILITY_PERFECT_FORWARD_HPP
#define INCLUDED_MEEVAX_UTILITY_PERFECT_FORWARD_HPP

#include <utility>

#define Perfect_Forward(X) std::forward<decltype(X)>(X)

#define Define_Perfect_Forwarding(FROM, TO)                                    \
template <typename... Ts>                                                      \
constexpr auto FROM(Ts&&... xs)                                                \
  noexcept(noexcept(TO(Perfect_Forward(xs)...)))                               \
  -> decltype(auto)                                                            \
{                                                                              \
  return TO(Perfect_Forward(xs)...);                                           \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#define Define_Const_Perfect_Forwarding(FROM, TO)                              \
template <typename... Ts>                                                      \
constexpr auto FROM(Ts&&... xs)                                                \
  const noexcept(noexcept(TO(Perfect_Forward(xs)...)))                         \
  -> decltype(auto)                                                            \
{                                                                              \
  return TO(Perfect_Forward(xs)...);                                           \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#define Define_Static_Perfect_Forwarding(FROM, TO)                             \
template <typename... Ts>                                                      \
static constexpr auto FROM(Ts&&... xs)                                         \
  noexcept(noexcept(TO(Perfect_Forward(xs)...)))                               \
  -> decltype(auto)                                                            \
{                                                                              \
  return TO(Perfect_Forward(xs)...);                                           \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#endif // INCLUDED_MEEVAX_UTILITY_PERFECT_FORWARD_HPP

