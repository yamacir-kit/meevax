#ifndef INCLUDED_MEEVAX_UTILITY_REQUIRES_HPP
#define INCLUDED_MEEVAX_UTILITY_REQUIRES_HPP

#include <type_traits>

#define Requires(...) \
  typename = typename std::enable_if< \
                        std::conjunction<__VA_ARGS__>::value \
                      >::type

#endif // INCLUDED_MEEVAX_UTILITY_REQUIRES_HPP

