#ifndef INCLUDED_MEEVAX_FUNCTIONAL_OPERATION_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_OPERATION_HPP

#include <functional>

#include <meevax/functional/identity.hpp>

namespace meevax { inline namespace functional
{
  /* ---- Binary Operations ------------------------------------------------- */

  using addition = std::plus<void>;

  using subtraction = std::minus<void>;

  using multiplication = std::multiplies<void>;

  using division = std::divides<void>;

  using modulo = std::modulus<void>;

  // TODO addition assignment
  // TODO subtraction assignment
  // TODO multiplication assignment
  // TODO division assignment
  // TODO modulo assignment

  /* ---- Unary Operations -------------------------------------------------- */

  struct unary_plus
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const -> decltype(auto)
    {
      return +std::forward<decltype(x)>(x);
    }
  };

  using unary_minus = std::negate<void>;

  struct prefix_increment
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const -> decltype(auto)
    {
      return ++std::forward<decltype(x)>(x);
    }
  };

  struct prefix_decrement
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const -> decltype(auto)
    {
      return --std::forward<decltype(x)>(x);
    }
  };

  struct postfix_increment
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const -> decltype(auto)
    {
      return std::forward<decltype(x)>(x)++;
    }
  };

  struct postfix_decrement
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const -> decltype(auto)
    {
      return std::forward<decltype(x)>(x)--;
    }
  };

  /* ---- Comparisons ------------------------------------------------------- */

  using equal_to = std::equal_to<void>;

  using not_equal_to = std::not_equal_to<void>;

  using less_than = std::less<void>;

  using less_than_or_equal_to = std::less_equal<void>;

  using greater_than = std::greater<void>;

  using greater_than_or_equal_to = std::greater_equal<void>;

  /* ---- Stream Input/Output ----------------------------------------------- */

  struct read
  {
    template <typename Port, typename... Ts>
    constexpr auto operator ()(Port&& port, Ts&&... xs) const -> decltype(auto)
    {
      return (port >> ... >> xs);
    }
  };

  struct write
  {
    template <typename Port, typename... Ts>
    constexpr auto operator ()(Port&& port, Ts&&... xs) const -> decltype(auto)
    {
      return (port << ... << xs);
    }
  };
}} // namespace meevax::functional

#endif // INCLUDED_MEEVAX_FUNCTIONAL_OPERATION_HPP
