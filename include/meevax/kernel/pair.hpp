#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/exception.hpp>
#include <meevax/kernel/object.hpp>

namespace meevax { inline namespace kernel
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
    , public identity<pair>
  {
    using std::pair<object, object>::pair;

    explicit pair()
      : std::pair<object, object> { unit, unit }
    {}

    virtual ~pair() = default;
  };

  /* ==== Pair Accessor ========================================================
  *
  * Pair accessors are not only for pair type. Accessing car and cdr is a valid
  * operation for everyone except the empty list.
  *
  * ========================================================================= */
  // auto car = [](const object& x) noexcept -> decltype(auto)
  // {
  //   return std::get<0>(x.binding());
  // };
  //
  // auto cdr = [](const object& x) noexcept -> decltype(auto)
  // {
  //   return std::get<1>(x.binding());
  // };

  // auto car = [](auto&& x) noexcept -> decltype(auto)
  // {
  //   if constexpr (std::is_base_of<pair, typename std::decay<decltype(x)>::type>::value)
  //   {
  //     return std::get<0>(std::forward<decltype(x)>(x));
  //   }
  //   else
  //   {
  //     return std::get<0>(x.binding());
  //   }
  // };
  //
  // auto cdr = [](auto&& x) noexcept -> decltype(auto)
  // {
  //   if constexpr (std::is_base_of<pair, typename std::decay<decltype(x)>::type>::value)
  //   {
  //     return std::get<1>(std::forward<decltype(x)>(x));
  //   }
  //   else
  //   {
  //     return std::get<1>(x.binding());
  //   }
  // };

  auto car = [](auto&& x) noexcept -> decltype(auto)
  {
    if constexpr (std::is_base_of<object, typename std::decay<decltype(x)>::type>::value)
    {
      return std::get<0>(x.binding());
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
    else
    {
      return std::get<1>(std::forward<decltype(x)>(x));
    }
  };

  /* ==== Pairs and Lists External Representation ==============================
  *
  * TODO documentation
  *
  *========================================================================== */
  auto operator<<(std::ostream& os, const pair& pare) -> decltype(os)
  {
    os << console::magenta << "(" << console::reset << car(pare);

    for (auto object { cdr(pare) }; object; object = cdr(object))
    {
      if (object.is<pair>())
      {
        os << " " << car(object);
      }
      else // iter is the last element of dotted-list.
      {
        os << console::magenta << " . " << console::reset << object;
      }
    }

    return os << console::magenta << ")" << console::reset;
  }
}} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP

