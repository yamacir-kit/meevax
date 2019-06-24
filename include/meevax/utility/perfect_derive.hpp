#ifndef INCLUDED_MEEVAX_UTILITY_PERFECT_DERIVE_HPP
#define INCLUDED_MEEVAX_UTILITY_PERFECT_DERIVE_HPP

#include <utility>

#define PERFECT_DERIVE(NAME, ACCESS_SPECIFIER, ...)                            \
  struct NAME                                                                  \
    : ACCESS_SPECIFIER __VA_ARGS__                                             \
  {                                                                            \
    template <typename... Ts>                                                  \
    explicit constexpr NAME(Ts&&... args)                                      \
      : __VA_ARGS__ {std::forward<Ts>(args)...}                                \
    {}                                                                         \
  };

#endif // INCLUDED_MEEVAX_UTILITY_PERFECT_DERIVE_HPP

