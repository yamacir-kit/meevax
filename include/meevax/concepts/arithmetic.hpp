#ifndef INCLUDED_MEEVAX_CONCEPTS_ARITHMETIC_HPP
#define INCLUDED_MEEVAX_CONCEPTS_ARITHMETIC_HPP

#include <meevax/type_traits/void_t.hpp>

namespace meevax { inline namespace concepts
{
  #define boilerplate(NAME, SYMBOL)                                            \
  template <typename T, typename U, typename = void>                           \
  struct NAME                                                                  \
    : public std::false_type                                                   \
  {};                                                                          \
                                                                               \
  template <typename T, typename U>                                            \
  struct NAME<T, U, type_traits::void_t<decltype(std::declval<const T&>() SYMBOL std::declval<const U&>())>> \
    : public std::true_type                                                    \
  {}

  // TODO RENAME TO 'supports_##FUNCTOR_NAME##_operation'

  boilerplate(addable, +);
  boilerplate(divisible, /);
  boilerplate(multipliable, *);
  boilerplate(subtractable, -);
  boilerplate(supports_modulo_operation, %);

  boilerplate(greater_equal_comparable, >=);
  boilerplate(greater_than_comparable, >);
  boilerplate(less_equal_comparable, <=);
  boilerplate(less_than_comparable, <);

  #undef boilerplate
}} // namespace meevax::concepts

#endif // INCLUDED_MEEVAX_CONCEPTS_ARITHMETIC_HPP
