#ifndef INCLUDED_MEEVAX_UTILITY_MODULE_HPP
#define INCLUDED_MEEVAX_UTILITY_MODULE_HPP

#include <utility>

// Curiously Recurring Template Pattern (CRTP)

#define IMPORT(M, SYMBOL, ...)                                                 \
template <typename... Ts>                                                      \
constexpr decltype(auto) SYMBOL(Ts&&... xs) __VA_ARGS__                        \
{                                                                              \
  return static_cast<M __VA_ARGS__&>(*this).M::SYMBOL(std::forward<decltype(xs)>(xs)...); \
}                                                                              \
static_assert(true)

#define EXPORT(M, SYMBOL) using M::SYMBOL

#endif // INCLUDED_MEEVAX_UTILITY_MODULE_HPP

