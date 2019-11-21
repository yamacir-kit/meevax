#ifndef INCLUDED_MEEVAX_KERNEL_OBJECT_HPP
#define INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

#include <meevax/kernel/pointer.hpp>

namespace meevax::kernel
{
  /* ==== The Pair Type =======================================================
  *
  * The pair type is always underlies any object type (is performance hack).
  *
  * We implemented heterogenous pointer by type-erasure, this is very flexible
  * but, requires dynamic-cast to restore erased type in any case. So, we
  * decided to remove typecheck for pair type, by always waste memory space
  * for two heterogenous pointer slot (yes, is cons-cell). If pair selector
  * (car/cdr) always requires typecheck, our kernel will be unbearlably slowly.
  * Built-in types are designed to make the best possible use of the fact that
  * these are pair as well (e.g. closure is pair of expression and lexical
  * environment, string is linear-list of character, complex, rational).
  *
  *========================================================================= */
  struct pair;

  using object = pointer<pair>;

  template <typename T, typename... Ts>
  constexpr decltype(auto) make(Ts&&... operands)
  {
    return
      object::bind<T>(
        std::forward<decltype(operands)>(operands)...);
  }

  extern "C" const object unit, unbound, undefined, unspecified;

  #define DERIVE(DERIVED, ACCESS, BASE)                                        \
  struct DERIVED                                                               \
    : ACCESS BASE                                                              \
  {                                                                            \
    template <typename... Ts>                                                  \
    explicit constexpr DERIVED(Ts&&... operands)                               \
      : BASE {std::forward<decltype(operands)>(operands)...}                   \
    {}                                                                         \
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_OBJECT_HPP

