#ifndef INCLUDED_MEEVAX_TYPE_TRAITS_IF_CONSTEXPR_HPP
#define INCLUDED_MEEVAX_TYPE_TRAITS_IF_CONSTEXPR_HPP

#include <sstream>
#include <stdexcept>
#include <type_traits>

namespace meevax { inline namespace type_traits
{
  #define boilerplate(CONCEPT)                                                 \
  template <typename T, typename = void>                                       \
  struct if_##CONCEPT                                                          \
  {                                                                            \
    template <typename R, typename... Ts>                                      \
    static auto invoke(Ts&&...) -> R                                           \
    {                                                                          \
      std::stringstream port {};                                               \
      port << typeid(T).name() << " is not " #CONCEPT;                         \
      throw std::logic_error { port.str() };                                   \
    }                                                                          \
  };                                                                           \
                                                                               \
  template <typename T>                                                        \
  struct if_##CONCEPT<T, typename std::enable_if<std::CONCEPT<T>::value>::type> \
  {                                                                            \
    template <typename R, typename F, typename... Ts>                          \
    static auto invoke(F&& f, Ts&&... xs) -> R                                 \
    {                                                                          \
      return f(std::forward<decltype(xs)>(xs)...);                             \
    }                                                                          \
  }

  boilerplate(is_copy_constructible);

  #undef boilerplate
}} // namespace meevax::type_traits

#endif // INCLUDED_MEEVAX_TYPE_TRAITS_IF_CONSTEXPR_HPP
