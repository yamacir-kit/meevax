#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/object.hpp>

namespace meevax
{
inline namespace kernel
{
  /* ---- Pair -----------------------------------------------------------------
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
   * ------------------------------------------------------------------------ */
  struct pair
    : public std::pair<object, object>
    , public identity<pair>
  {
    using std::pair<object, object>::pair;

    explicit pair()
      : std::pair<object, object> { unit, unit }
    {}

    virtual ~pair() = default;
  };

  auto operator<<(std::ostream & port, pair const&) -> decltype(port);

  /* ---- Pair Accessor --------------------------------------------------------
   *
   *  Pair accessors are not only for pair type. Accessing car and cdr is a
   *  valid operation for everyone except the empty list.
   *
   * ------------------------------------------------------------------------ */
  auto car = [](auto&& x) noexcept -> decltype(auto)
  {
    if constexpr (std::is_base_of<object, typename std::decay<decltype(x)>::type>::value)
    {
      return std::get<0>(x.binding());
    }
    else if constexpr (std::is_base_of<std::reference_wrapper<object const>, typename std::decay<decltype(x)>::type>::value)
    {
      return std::get<0>(x.get().binding());
    }
    else
    {
      return std::get<0>(std::forward<decltype(x)>(x));
    }
  };

  auto cdr = [](auto&& x) noexcept -> decltype(auto)
  {
    if constexpr (std::is_base_of<object, typename std::decay<decltype(x)>::type>::value)
    {
      return std::get<1>(x.binding());
    }
    else if constexpr (std::is_base_of<std::reference_wrapper<object const>, typename std::decay<decltype(x)>::type>::value)
    {
      return std::get<1>(x.get().binding());
    }
    else
    {
      return std::get<1>(std::forward<decltype(x)>(x));
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
