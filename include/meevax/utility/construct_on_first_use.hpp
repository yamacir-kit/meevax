#ifndef INCLUDED_MEEVAX_UTILITY_CONSTRUCT_ON_FIRST_USE_HPP
#define INCLUDED_MEEVAX_UTILITY_CONSTRUCT_ON_FIRST_USE_HPP

#define Construct_On_First_Use(NAME, ...)                                      \
static constexpr auto NAME = []() -> decltype(auto)                            \
{                                                                              \
  static const auto x { __VA_ARGS__ };                                         \
  return x;                                                                    \
}

#endif // INCLUDED_MEEVAX_UTILITY_CONSTRUCT_ON_FIRST_USE_HPP

