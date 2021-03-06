#ifndef INCLUDED_MEEVAX_FUNCTIONAL_OPERATOR_HPP
#define INCLUDED_MEEVAX_FUNCTIONAL_OPERATOR_HPP

#include <ostream>

#include <meevax/functional/identity.hpp>
#include <meevax/utility/overload.hpp>

namespace meevax
{
  #define DEFINE_OPERATOR(NAME, BASE)                                          \
  struct NAME : public BASE                                                    \
  {                                                                            \
    friend auto operator <<(std::ostream & os, NAME const&) -> std::ostream &  \
    {                                                                          \
      return os << #NAME;                                                      \
    }                                                                          \
  }

  DEFINE_OPERATOR(add, std::plus<void>);

  #undef DEFINE_OPERATOR

  // auto add                      = overload([](auto&& a, auto&& b) constexpr { return a +  b; });
  // auto multiply                 = overload([](auto&& a, auto&& b) constexpr { return a *  b; });
  // auto subtract                 = overload([](auto&& a, auto&& b) constexpr { return a -  b; });
  // auto divide                   = overload([](auto&& a, auto&& b) constexpr { return a /  b; });
  // auto modulo                   = overload([](auto&& a, auto&& b) constexpr { return a %  b; });
  //
  // auto                 equal_to = overload([](auto&& a, auto&& b) constexpr { return a == b; });
  // auto             not_equal_to = overload([](auto&& a, auto&& b) constexpr { return a != b; });
  // auto    less_than_or_equal_to = overload([](auto&& a, auto&& b) constexpr { return a <= b; });
  // auto    less_than             = overload([](auto&& a, auto&& b) constexpr { return a <  b; });
  // auto greater_than_or_equal_to = overload([](auto&& a, auto&& b) constexpr { return a >= b; });
  // auto greater_than             = overload([](auto&& a, auto&& b) constexpr { return a >  b; });
  //
  // auto operator <<(std::ostream & port, decltype(add                     ) const&) -> std::ostream & { return port << "add"; }
  // auto operator <<(std::ostream & port, decltype(multiply                ) const&) -> std::ostream & { return port << "multiply"; }
  // auto operator <<(std::ostream & port, decltype(subtract                ) const&) -> std::ostream & { return port << "subtract"; }
  // auto operator <<(std::ostream & port, decltype(divide                  ) const&) -> std::ostream & { return port << "divide"; }
  // auto operator <<(std::ostream & port, decltype(modulo                  ) const&) -> std::ostream & { return port << "modulo"; }
  //
  // auto operator <<(std::ostream & port, decltype(                equal_to) const&) -> std::ostream & { return port << "equal_to"; }
  // auto operator <<(std::ostream & port, decltype(            not_equal_to) const&) -> std::ostream & { return port << "not_equal_to"; }
  // auto operator <<(std::ostream & port, decltype(   less_than_or_equal_to) const&) -> std::ostream & { return port << "less_than_or_equal_to"; }
  // auto operator <<(std::ostream & port, decltype(   less_than            ) const&) -> std::ostream & { return port << "less_than"; }
  // auto operator <<(std::ostream & port, decltype(greater_than_or_equal_to) const&) -> std::ostream & { return port << "greater_than_or_equal_to"; }
  // auto operator <<(std::ostream & port, decltype(greater_than            ) const&) -> std::ostream & { return port << "greater_than"; }
} // namespace meevax

#endif // INCLUDED_MEEVAX_FUNCTIONAL_OPERATOR_HPP
