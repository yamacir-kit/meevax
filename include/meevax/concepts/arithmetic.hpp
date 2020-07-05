#ifndef INCLUDED_MEEVAX_CONCEPTS_ARITHMETIC_HPP
#define INCLUDED_MEEVAX_CONCEPTS_ARITHMETIC_HPP

#include <type_traits>

namespace meevax::concepts
{
  #define DEFINE_BINARY_ARITHMETIC_CONCEPT(NAME, SYMBOL)                       \
  template <typename T, typename U, typename = void>                           \
  struct NAME                                                                  \
    : public std::false_type                                                   \
  {};                                                                          \
                                                                               \
  template <typename T, typename U>                                            \
  struct NAME<T, U, std::void_t<decltype(                                      \
           std::declval<T>().operator SYMBOL(std::declval<U>())                \
           )>>                                                                 \
    : public std::true_type                                                    \
  {}

  DEFINE_BINARY_ARITHMETIC_CONCEPT(addable, +);
  DEFINE_BINARY_ARITHMETIC_CONCEPT(divisible, /);
  DEFINE_BINARY_ARITHMETIC_CONCEPT(multipliable, *);
  DEFINE_BINARY_ARITHMETIC_CONCEPT(subtractable, -);

  DEFINE_BINARY_ARITHMETIC_CONCEPT(greater_equal_comparable, >=);
  DEFINE_BINARY_ARITHMETIC_CONCEPT(greater_than_comparable, >);
  DEFINE_BINARY_ARITHMETIC_CONCEPT(less_equal_comparable, <=);
  DEFINE_BINARY_ARITHMETIC_CONCEPT(less_than_comparable, <);
} // namespace meevax::concepts

#undef DEFINE_BINARY_ARITHMETIC_CONCEPT
#endif // INCLUDED_MEEVAX_CONCEPTS_ARITHMETIC_HPP
