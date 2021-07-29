#ifndef INCLUDED_MEEVAX_UTILITY_MODULE_HPP
#define INCLUDED_MEEVAX_UTILITY_MODULE_HPP

#include <utility>

#define IMPORT(MODULE, SYMBOL, ...)                                            \
template <typename... Ts>                                                      \
auto SYMBOL(Ts&&... xs) __VA_ARGS__ -> decltype(auto)                          \
{                                                                              \
  return static_cast<MODULE __VA_ARGS__&>(*this).MODULE::SYMBOL(               \
    std::forward<decltype(xs)>(xs)...);                                        \
}                                                                              \
static_assert(true)

#define EXPORT(M, SYMBOL) using M::SYMBOL

#endif // INCLUDED_MEEVAX_UTILITY_MODULE_HPP
