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
  #if __cpp_if_constexpr

  auto car = [](auto&& pare) noexcept -> decltype(auto)
  {
    if constexpr (std::is_base_of<object, typename std::decay<decltype(pare)>::type>::value)
    {
      return std::get<0>(pare.binding());
    }
    else
    {
      return std::get<0>(std::forward<decltype(pare)>(pare));
    }
  };

  auto cdr = [](auto&& pare) noexcept -> decltype(auto)
  {
    if constexpr (std::is_base_of<object, typename std::decay<decltype(pare)>::type>::value)
    {
      return std::get<1>(pare.binding());
    }
    else
    {
      return std::get<1>(std::forward<decltype(pare)>(pare));
    }
  };

  #else // __cpp_if_constexpr

  template <typename T, typename = void>
  struct generic_accessor
  {
    static auto car(const T& pare) -> decltype(auto) { return std::get<0>(pare); }
    static auto cdr(const T& pare) -> decltype(auto) { return std::get<1>(pare); }
  };

  template <typename T>
  struct generic_accessor<T, typename std::enable_if<std::is_base_of<object, typename std::decay<T>::type>::value>::type>
  {
    static auto car(const T& pare) -> decltype(auto) { return std::get<0>(pare.binding()); }
    static auto cdr(const T& pare) -> decltype(auto) { return std::get<1>(pare.binding()); }
  };

  auto car = [](auto&& pare) noexcept -> decltype(auto)
  {
    return generic_accessor<decltype(pare)>::car(pare);
  };

  auto cdr = [](auto&& pare) noexcept -> decltype(auto)
  {
    return generic_accessor<decltype(pare)>::cdr(pare);
  };

  #endif // __cpp_if_constexpr

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
