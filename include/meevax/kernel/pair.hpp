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
    using std::pair<object, object>::pair;

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
  #define DEFINE_PAIR_ACCESSOR(SYMBOL, INDEX)                                  \
  inline decltype(auto) SYMBOL(const object& o)                                \
  {                                                                            \
    assert(o);                                                                 \
    return std::get<INDEX>(o.dereference());                                   \
  }

  DEFINE_PAIR_ACCESSOR(car, 0)
  DEFINE_PAIR_ACCESSOR(cdr, 1)

  /* ==== Pairs and Lists External Representation ==============================
  *
  * TODO documentation
  *
  *========================================================================== */
  auto operator<<(std::ostream& os, const pair& pare)
    -> decltype(os)
  {
    os << posix::highlight::syntax << "("
       << posix::attribute::normal << std::get<0>(pare);

    for (auto object {std::get<1>(pare)}; object; object = cdr(object))
    {
      if (object.is<pair>())
      {
        os << " " << car(object);
      }
      else // iter is the last element of dotted-list.
      {
        os << posix::highlight::syntax << " . "
           << posix::attribute::normal << object;
      }
    }

    return os << posix::highlight::syntax << ")"
              << posix::attribute::normal;
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP

