#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/object.hpp>

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
  *========================================================================== */
  struct pair
    : public std::pair<object, object>
    , public objective<pair>
  {
    template <typename... Ts>
    explicit constexpr pair(Ts&&... operands)
      : std::pair<object, object> {std::forward<decltype(operands)>(operands)...}
    {}

    pair()
      : std::pair<object, object> {unit, unit}
    {}

    virtual ~pair() = default;
  };

  #ifndef NDEBUG
  #define DEFINE_SELECTOR(NAME, INDEX)                                         \
  decltype(auto) NAME(const object& object)                                    \
  {                                                                            \
    if (object)                                                                \
    {                                                                          \
      return std::get<INDEX>(object.dereference());                            \
    }                                                                          \
    else                                                                       \
    {                                                                          \
      throw kernel_error_about_pair {                                          \
        "internal illegal selection rejected"                                  \
      };                                                                       \
    }                                                                          \
  }
  #else
  #define DEFINE_SELECTOR(NAME, INDEX)                                         \
  decltype(auto) NAME(const object& object)                                    \
  {                                                                            \
    return std::get<INDEX>(object.dereference());                              \
  }
  #endif // NDEBUG

  DEFINE_SELECTOR(car, 0)
  DEFINE_SELECTOR(cdr, 1)

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

