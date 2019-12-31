#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/object.hpp>

namespace meevax::kernel
{
  /* ==== The Pair Type ========================================================
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
  *========================================================================== */
  struct pair
    : public std::pair<object, object>
    , public objective<pair>
  {
    template <typename... Ts>
    explicit constexpr pair(Ts&&... operands)
      : std::pair<object, object> {std::forward<decltype(operands)>(operands)...}
    {}

    explicit pair()
      : std::pair<object, object> {unit, unit}
    {}

    virtual ~pair() = default;
  };

  /* ==== Pair Accessor ========================================================
  *
  * Pair accessors are not only for pair type. Accessing car and cdr is a valid
  * operation for everyone except the empty list.
  *
  *========================================================================== */
  #define DEFINE_PAIR_ACCESSOR(IDENTIFIER, INDEX)                              \
  inline decltype(auto) IDENTIFIER(const object& o)                            \
  {                                                                            \
    assert(o);                                                                 \
    return std::get<INDEX>(o.dereference());                                   \
  }

  DEFINE_PAIR_ACCESSOR(car, 0)
  DEFINE_PAIR_ACCESSOR(cdr, 1)

  /* ==== Pair Mutator =========================================================
  *
  * TODO documentation
  *
  *========================================================================== */
  #define DEFINE_PAIR_MUTATOR(IDENTIFIER, ACCESSOR)                            \
  template <typename... Ts>                                                    \
  inline decltype(auto) IDENTIFIER(const object& x, Ts&&... xs)                \
  {                                                                            \
    return                                                                     \
      std::atomic_store(                                                       \
        &ACCESSOR(x),                                                          \
        std::forward<decltype(xs)>(xs)...);                                    \
  }

  DEFINE_PAIR_MUTATOR(set_car, car)
  DEFINE_PAIR_MUTATOR(set_cdr, cdr)

  /* ==== Pairs and Lists External Representation ==============================
  *
  * TODO documentation
  *
  *========================================================================== */
  auto operator<<(std::ostream& os, const pair& pare)
    -> decltype(os)
  {
    os << highlight::syntax << "(" << attribute::normal << std::get<0>(pare);

    for (auto object {std::get<1>(pare)}; object; object = cdr(object))
    {
      if (object.is<pair>())
      {
        os << " " << car(object);
      }
      else // iter is the last element of dotted-list.
      {
        os << highlight::syntax << " . " << attribute::normal << object;
      }
    }

    return os << highlight::syntax << ")" << attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP

