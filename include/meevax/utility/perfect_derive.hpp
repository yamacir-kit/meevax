#ifndef INCLUDED_MEEVAX_UTILITY_PERFECT_DERIVE_HPP
#define INCLUDED_MEEVAX_UTILITY_PERFECT_DERIVE_HPP

#include <utility>

#define PERFECT_DERIVE(NAME, ACCESS_SPECIFIER, BASE) \
  struct NAME \
    : ACCESS_SPECIFIER BASE \
  { \
    template <typename... Ts> \
    explicit constexpr NAME(Ts&&... args) \
      : BASE {std::forward<Ts>(args)...} \
    {} \
  };

#endif // INCLUDED_MEEVAX_UTILITY_PERFECT_DERIVE_HPP

