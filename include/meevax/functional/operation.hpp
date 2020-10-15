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

  template <typename... Ts>
  using unary_plus = identity<Ts...>;

  template <typename... Ts>
  using unary_minus = std::negate<Ts...>;

  // TODO increment
  // TODO decrement
}} // namespace meevax::functional

#endif // INCLUDED_MEEVAX_FUNCTIONAL_OPERATION_HPP
