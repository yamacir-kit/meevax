#ifndef INCLUDED_MEEVAX_SYSTEM_OBJECT_HPP
#define INCLUDED_MEEVAX_SYSTEM_OBJECT_HPP

#include <meevax/system/pointer.hpp>

namespace meevax::system
{
  struct pair;

  /**
   * The pair type is always underlies any object type (is performance hack).
   *
   * We implemented heterogenous pointer by type-erasure, this is very flexible
   * but, requires dynamic-cast to restore erased type in any case. So, we
   * decided to remove typecheck for pair type, by always waste memory space
   * for two heterogenous pointer slot (yes, is cons-cell). If pair selector
   * (car/cdr) always requires typecheck, our system will be unbearlably slowly.
   * Built-in types are designed to make the best possible use of the fact that
   * these are pair as well (e.g. closure is pair of expression and lexical
   * environment, string is linear-list of character, complex, rational).
   */
  using object = pointer<pair>;

  template <typename T, typename... Ts>
  constexpr decltype(auto) make(Ts&&... args)
  {
    return object::bind<T>(std::forward<Ts>(args)...);
  }

  extern "C" const object unit, unbound, undefined, unspecified;

  #define DERIVE(DERIVED, ACCESS, BASE)                                        \
  struct DERIVED                                                               \
    : ACCESS BASE                                                              \
  {                                                                            \
    template <typename... Objects>                                             \
    explicit constexpr DERIVED(Objects&&... object)                            \
      : BASE {std::forward<Objects>(object)...}                                \
    {}                                                                         \
  };
} // namespace meevax::system

#endif // INCLUDED_MEEVAX_SYSTEM_OBJECT_HPP

