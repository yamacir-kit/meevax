#ifndef INCLUDED_MEEVAX_UTILITY_MODULE_HPP
#define INCLUDED_MEEVAX_UTILITY_MODULE_HPP

#include <utility>

// Curiously Recurring Template Pattern (CRTP)

#define Import(FROM, SYMBOL)                                                   \
template <typename... Ts>                                                      \
constexpr decltype(auto) SYMBOL(Ts&&... xs)                                    \
{                                                                              \
  return                                                                       \
    static_cast<FROM&>(*this).FROM::SYMBOL(                                    \
      std::forward<decltype(xs)>(xs)...);                                      \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#define Import_Const(FROM, SYMBOL)                                             \
template <typename... Ts>                                                      \
constexpr decltype(auto) SYMBOL(Ts&&... xs) const                              \
{                                                                              \
  return                                                                       \
    static_cast<const FROM&>(*this).FROM::SYMBOL(                              \
      std::forward<decltype(xs)>(xs)...);                                      \
}                                                                              \
static_assert(true, "semicolon required after this macro")

#define Export(FROM, SYMBOL) using FROM::SYMBOL

#endif // INCLUDED_MEEVAX_UTILITY_MODULE_HPP

