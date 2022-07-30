/*
   Copyright 2018-2022 Tatsuya Yamasaki.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#include <functional>
#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  auto operator * (exact_integer const& a, exact_integer const& b) -> exact_integer { return exact_integer(mul, a, b); }
  auto operator + (exact_integer const& a, exact_integer const& b) -> exact_integer { return exact_integer(add, a, b); }
  auto operator - (exact_integer const& a, exact_integer const& b) -> exact_integer { return exact_integer(sub, a, b); }
  auto operator / (exact_integer const& a, exact_integer const& b) -> ratio         { return ratio(make(a), make(b)); }
  auto operator % (exact_integer const& a, exact_integer const& b) -> exact_integer { return exact_integer(mod, a, b); }
  auto operator !=(exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) != 0; }
  auto operator < (exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) <  0; }
  auto operator <=(exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) <= 0; }
  auto operator ==(exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) == 0; }
  auto operator > (exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) >  0; }
  auto operator >=(exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) >= 0; }

  auto operator * (exact_integer const& a, ratio const& b) -> ratio { return ratio(make(a * b.numerator().as<exact_integer>()), cdr(b)); }
  auto operator + (exact_integer const& a, ratio const& b) -> ratio { return ratio(make(a * b.denominator().as<exact_integer>() + b.numerator().as<exact_integer>()), cdr(b)); }
  auto operator - (exact_integer const& a, ratio const& b) -> ratio { return ratio(make(a * b.denominator().as<exact_integer>() - b.numerator().as<exact_integer>()), cdr(b)); }
  auto operator / (exact_integer const& a, ratio const& b) -> ratio { return a * b.invert(); }
  auto operator % (exact_integer const&  , ratio const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
  auto operator !=(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return x.is_integer() ? a != x.numerator().as<exact_integer>() : false; }
  auto operator < (exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return x.is_integer() ? a <  x.numerator().as<exact_integer>() : false; }
  auto operator <=(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return x.is_integer() ? a <= x.numerator().as<exact_integer>() : false; }
  auto operator ==(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return x.is_integer() ? a == x.numerator().as<exact_integer>() : false; }
  auto operator > (exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return x.is_integer() ? a >  x.numerator().as<exact_integer>() : false; }
  auto operator >=(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return x.is_integer() ? a >= x.numerator().as<exact_integer>() : false; }

  auto operator * (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() * b), cdr(a)); }
  auto operator + (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() + a.denominator().as<exact_integer>() * b), cdr(a)); }
  auto operator - (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() - a.denominator().as<exact_integer>() * b), cdr(a)); }
  auto operator / (ratio const& a, exact_integer const& b) -> ratio { return ratio(car(a), make(a.denominator().as<exact_integer>() * b)); }
  auto operator % (ratio const&  , exact_integer const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
  auto operator !=(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return x.is_integer() ? x.numerator().as<exact_integer>() != b : false; }
  auto operator < (ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return x.is_integer() ? x.numerator().as<exact_integer>() <  b : false; }
  auto operator <=(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return x.is_integer() ? x.numerator().as<exact_integer>() <= b : false; }
  auto operator ==(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return x.is_integer() ? x.numerator().as<exact_integer>() == b : false; }
  auto operator > (ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return x.is_integer() ? x.numerator().as<exact_integer>() >  b : false; }
  auto operator >=(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return x.is_integer() ? x.numerator().as<exact_integer>() >= b : false; }

  auto operator + (ratio const& a, ratio const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() * b.denominator().as<exact_integer>() + b.numerator().as<exact_integer>() * a.denominator().as<exact_integer>()), make(a.denominator().as<exact_integer>() * b.denominator().as<exact_integer>())); }
  auto operator - (ratio const& a, ratio const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() * b.denominator().as<exact_integer>() - b.numerator().as<exact_integer>() * a.denominator().as<exact_integer>()), make(a.denominator().as<exact_integer>() * b.denominator().as<exact_integer>())); }
  auto operator * (ratio const& a, ratio const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() * b.numerator().as<exact_integer>()), make(a.denominator().as<exact_integer>() * b.denominator().as<exact_integer>())); }
  auto operator / (ratio const& a, ratio const& b) -> ratio { return a * b.invert(); }
  auto operator % (ratio const&  , ratio const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
  auto operator ==(ratio const& a, ratio const& b) -> bool  { return (a.numerator().as<exact_integer>() * b.denominator().as<exact_integer>()) == (b.numerator().as<exact_integer>() * a.denominator().as<exact_integer>()); }
  auto operator !=(ratio const& a, ratio const& b) -> bool  { return (a.numerator().as<exact_integer>() * b.denominator().as<exact_integer>()) != (b.numerator().as<exact_integer>() * a.denominator().as<exact_integer>()); }
  auto operator < (ratio const& a, ratio const& b) -> bool  { return (a.numerator().as<exact_integer>() * b.denominator().as<exact_integer>()) <  (b.numerator().as<exact_integer>() * a.denominator().as<exact_integer>()); }
  auto operator <=(ratio const& a, ratio const& b) -> bool  { return (a.numerator().as<exact_integer>() * b.denominator().as<exact_integer>()) <= (b.numerator().as<exact_integer>() * a.denominator().as<exact_integer>()); }
  auto operator > (ratio const& a, ratio const& b) -> bool  { return (a.numerator().as<exact_integer>() * b.denominator().as<exact_integer>()) >  (b.numerator().as<exact_integer>() * a.denominator().as<exact_integer>()); }
  auto operator >=(ratio const& a, ratio const& b) -> bool  { return (a.numerator().as<exact_integer>() * b.denominator().as<exact_integer>()) >= (b.numerator().as<exact_integer>() * a.denominator().as<exact_integer>()); }

  namespace experimental
  {
    #define APPLY(OPERATOR, T, U) \
    { type_index<2>(typeid(T), typeid(U)), binary_operation<OPERATOR, T, U>() }

    #define DEFINE_OVERLOADINGS(NAME, OP)                                                                                                                       \
    std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> NAME                                                        \
    {                                                                                                                                                           \
      APPLY(OP, exact_integer, exact_integer), APPLY(OP, exact_integer, ratio), APPLY(OP, exact_integer, single_float), APPLY(OP, exact_integer, double_float), \
      APPLY(OP, ratio,         exact_integer), APPLY(OP, ratio,         ratio), APPLY(OP, ratio,         single_float), APPLY(OP, ratio,         double_float), \
      APPLY(OP, single_float,  exact_integer), APPLY(OP, single_float,  ratio), APPLY(OP, single_float,  single_float), APPLY(OP, single_float,  double_float), \
      APPLY(OP, double_float,  exact_integer), APPLY(OP, double_float,  ratio), APPLY(OP, double_float,  single_float), APPLY(OP, double_float,  double_float), \
    }

    DEFINE_OVERLOADINGS(add, std::plus      <void>);
    DEFINE_OVERLOADINGS(sub, std::minus     <void>);
    DEFINE_OVERLOADINGS(mul, std::multiplies<void>);
    DEFINE_OVERLOADINGS(div, std::divides   <void>);
    DEFINE_OVERLOADINGS(mod, std::modulus   <void>);

    DEFINE_OVERLOADINGS(equal_to,      std::equal_to     <void>);
    DEFINE_OVERLOADINGS(not_equal_to,  std::not_equal_to <void>);
    DEFINE_OVERLOADINGS(less,          std::less         <void>);
    DEFINE_OVERLOADINGS(less_equal,    std::less_equal   <void>);
    DEFINE_OVERLOADINGS(greater,       std::greater      <void>);
    DEFINE_OVERLOADINGS(greater_equal, std::greater_equal<void>);
  }

  auto operator +(const_reference x, const_reference y) -> value_type { return experimental::add.at(type_index<2>(x.type(), y.type()))(x, y); }
  auto operator -(const_reference x, const_reference y) -> value_type { return experimental::sub.at(type_index<2>(x.type(), y.type()))(x, y); }
  auto operator *(const_reference x, const_reference y) -> value_type { return experimental::mul.at(type_index<2>(x.type(), y.type()))(x, y); }
  auto operator /(const_reference x, const_reference y) -> value_type { return experimental::div.at(type_index<2>(x.type(), y.type()))(x, y); }
  auto operator %(const_reference x, const_reference y) -> value_type { return experimental::mod.at(type_index<2>(x.type(), y.type()))(x, y); }
} // namespace kernel
} // namespace meevax
