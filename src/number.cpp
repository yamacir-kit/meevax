#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator * (exact_integer const& a, object const& b) -> object { return apply      ([](auto&& a, auto&& b) { return a *  b; }, a, b); }
  auto operator + (exact_integer const& a, object const& b) -> object { return apply      ([](auto&& a, auto&& b) { return a +  b; }, a, b); }
  auto operator - (exact_integer const& a, object const& b) -> object { return apply      ([](auto&& a, auto&& b) { return a -  b; }, a, b); }
  auto operator / (exact_integer const& a, object const& b) -> object { return apply      ([](auto&& a, auto&& b) { return a /  b; }, a, b); }
  auto operator % (exact_integer const& a, object const& b) -> object { return apply      ([](auto&& a, auto&& b) { return a %  b; }, a, b); }
  auto operator !=(exact_integer const& a, object const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a != b; }, a, b); }
  auto operator < (exact_integer const& a, object const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a <  b; }, a, b); }
  auto operator <=(exact_integer const& a, object const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a <= b; }, a, b); }
  auto operator ==(exact_integer const& a, object const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a == b; }, a, b); }
  auto operator > (exact_integer const& a, object const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a >  b; }, a, b); }
  auto operator >=(exact_integer const& a, object const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a >= b; }, a, b); }

  auto operator * (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value *  b.value); }
  auto operator + (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value +  b.value); }
  auto operator - (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value -  b.value); }
  auto operator / (exact_integer const& a, exact_integer const& b) -> ratio         { return ratio(make(a), make(b)); }
  auto operator % (exact_integer const& a, exact_integer const& b) -> exact_integer { return static_cast<exact_integer>(a.value %  b.value); }
  auto operator !=(exact_integer const& a, exact_integer const& b) -> boolean       { return                            a.value != b.value ; }
  auto operator < (exact_integer const& a, exact_integer const& b) -> boolean       { return                            a.value <  b.value ; }
  auto operator <=(exact_integer const& a, exact_integer const& b) -> boolean       { return                            a.value <= b.value ; }
  auto operator ==(exact_integer const& a, exact_integer const& b) -> boolean       { return                            a.value == b.value ; }
  auto operator > (exact_integer const& a, exact_integer const& b) -> boolean       { return                            a.value >  b.value ; }
  auto operator >=(exact_integer const& a, exact_integer const& b) -> boolean       { return                            a.value >= b.value ; }

  auto operator * (exact_integer const& a, ratio const& b) -> ratio { return ratio(make(a * b.numerator()), cdr(b)); }
  auto operator + (exact_integer const& a, ratio const& b) -> ratio { return ratio(make(a * b.denominator() + b.numerator()), cdr(b)); }
  auto operator - (exact_integer const& a, ratio const& b) -> ratio { return ratio(make(a * b.denominator() - b.numerator()), cdr(b)); }
  auto operator / (exact_integer const& a, ratio const& b) -> ratio { return a * b.invert(); }
  auto operator % (exact_integer const&  , ratio const&  ) -> ratio { throw error(__FILE__, ":", __LINE__); }
  auto operator !=(exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a != x.numerator() : boolean(false); }
  auto operator < (exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a <  x.numerator() : boolean(false); }
  auto operator <=(exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a <= x.numerator() : boolean(false); }
  auto operator ==(exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a == x.numerator() : boolean(false); }
  auto operator > (exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a >  x.numerator() : boolean(false); }
  auto operator >=(exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a >= x.numerator() : boolean(false); }

  auto operator * (ratio const& a, object const& b) -> object  { return apply         ([](auto&& a, auto&& b) { return a *  b; }, a, b); }
  auto operator + (ratio const& a, object const& b) -> object  { return apply         ([](auto&& a, auto&& b) { return a +  b; }, a, b); }
  auto operator - (ratio const& a, object const& b) -> object  { return apply         ([](auto&& a, auto&& b) { return a -  b; }, a, b); }
  auto operator / (ratio const& a, object const& b) -> object  { return apply         ([](auto&& a, auto&& b) { return a /  b; }, a, b); }
  auto operator % (ratio const& a, object const& b) -> object  { return apply         ([](auto&& a, auto&& b) { return a %  b; }, a, b); }
  auto operator !=(ratio const& a, object const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a != b; }, a, b); }
  auto operator < (ratio const& a, object const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a <  b; }, a, b); }
  auto operator <=(ratio const& a, object const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a <= b; }, a, b); }
  auto operator ==(ratio const& a, object const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a == b; }, a, b); }
  auto operator > (ratio const& a, object const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a >  b; }, a, b); }
  auto operator >=(ratio const& a, object const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a >= b; }, a, b); }

  auto operator * (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator() * b), cdr(a)); }
  auto operator + (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator() + a.denominator() * b), cdr(a)); }
  auto operator - (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator() - a.denominator() * b), cdr(a)); }
  auto operator / (ratio const& a, exact_integer const& b) -> ratio { return ratio(car(a), make(a.denominator() * b)); }
  auto operator % (ratio const&  , exact_integer const&  ) -> ratio { throw error(__FILE__, ":", __LINE__); }
  auto operator !=(ratio const& a, exact_integer const& b) -> boolean { auto const x = a.reduce(); return x.is_integer() ? x.numerator() != b : boolean(false); }
  auto operator < (ratio const& a, exact_integer const& b) -> boolean { auto const x = a.reduce(); return x.is_integer() ? x.numerator() <  b : boolean(false); }
  auto operator <=(ratio const& a, exact_integer const& b) -> boolean { auto const x = a.reduce(); return x.is_integer() ? x.numerator() <= b : boolean(false); }
  auto operator ==(ratio const& a, exact_integer const& b) -> boolean { auto const x = a.reduce(); return x.is_integer() ? x.numerator() == b : boolean(false); }
  auto operator > (ratio const& a, exact_integer const& b) -> boolean { auto const x = a.reduce(); return x.is_integer() ? x.numerator() >  b : boolean(false); }
  auto operator >=(ratio const& a, exact_integer const& b) -> boolean { auto const x = a.reduce(); return x.is_integer() ? x.numerator() >= b : boolean(false); }

  auto operator + (ratio const& a, ratio const& b) -> ratio   { return ratio(make(a.numerator() * b.denominator() + b.numerator() * a.denominator()), make(a.denominator() * b.denominator())); }
  auto operator - (ratio const& a, ratio const& b) -> ratio   { return ratio(make(a.numerator() * b.denominator() - b.numerator() * a.denominator()), make(a.denominator() * b.denominator())); }
  auto operator * (ratio const& a, ratio const& b) -> ratio   { return ratio(make(a.numerator() * b.numerator()), make(a.denominator() * b.denominator())); }
  auto operator / (ratio const& a, ratio const& b) -> ratio   { return a * b.invert(); }
  auto operator % (ratio const&  , ratio const&  ) -> ratio   { throw error(__FILE__, ":", __LINE__); }
  auto operator ==(ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) == (b.numerator() * a.denominator()); }
  auto operator !=(ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) != (b.numerator() * a.denominator()); }
  auto operator < (ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) <  (b.numerator() * a.denominator()); }
  auto operator <=(ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) <= (b.numerator() * a.denominator()); }
  auto operator > (ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) >  (b.numerator() * a.denominator()); }
  auto operator >=(ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) >= (b.numerator() * a.denominator()); }
} // namespace kernel
} // namespace meevax
