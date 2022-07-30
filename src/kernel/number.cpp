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

#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  auto exact_integer::operator * (const_reference b) const -> value_type { return apply(mul, *this, b); }
  auto exact_integer::operator + (const_reference b) const -> value_type { return apply(add, *this, b); }
  auto exact_integer::operator - (const_reference b) const -> value_type { return apply(sub, *this, b); }
  auto exact_integer::operator / (const_reference b) const -> value_type { return apply(div, *this, b); }
  auto exact_integer::operator % (const_reference b) const -> value_type { return apply(mod, *this, b); }
  auto exact_integer::operator !=(const_reference b) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a != b; }, *this, b); }
  auto exact_integer::operator < (const_reference b) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a <  b; }, *this, b); }
  auto exact_integer::operator <=(const_reference b) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a <= b; }, *this, b); }
  auto exact_integer::operator ==(const_reference b) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a == b; }, *this, b); }
  auto exact_integer::operator > (const_reference b) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a >  b; }, *this, b); }
  auto exact_integer::operator >=(const_reference b) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a >= b; }, *this, b); }

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

  auto ratio::operator * (const_reference x) const -> value_type { return apply(mul, *this, x); }
  auto ratio::operator + (const_reference x) const -> value_type { return apply(add, *this, x); }
  auto ratio::operator - (const_reference x) const -> value_type { return apply(sub, *this, x); }
  auto ratio::operator / (const_reference x) const -> value_type { return apply(div, *this, x); }
  auto ratio::operator % (const_reference x) const -> value_type { return apply(mod, *this, x); }
  auto ratio::operator !=(const_reference x) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a != b; }, *this, x); }
  auto ratio::operator < (const_reference x) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a <  b; }, *this, x); }
  auto ratio::operator <=(const_reference x) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a <= b; }, *this, x); }
  auto ratio::operator ==(const_reference x) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a == b; }, *this, x); }
  auto ratio::operator > (const_reference x) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a >  b; }, *this, x); }
  auto ratio::operator >=(const_reference x) const -> bool       { return apply<bool>([](auto&& a, auto&& b) { return a >= b; }, *this, x); }

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
    #define ADD(T, U) { type_index<2>(typeid(T), typeid(U)), binary_operation<std::plus<void>, T, U>() }

    std::unordered_map<type_index<2>, std::function<value_type (const_reference, const_reference)>> add
    {
      ADD(exact_integer, exact_integer), ADD(exact_integer, ratio), ADD(exact_integer, single_float), ADD(exact_integer, double_float),
      ADD(ratio,         exact_integer), ADD(ratio,         ratio), ADD(ratio,         single_float), ADD(ratio,         double_float),
      ADD(single_float,  exact_integer), ADD(single_float,  ratio), ADD(single_float,  single_float), ADD(single_float,  double_float),
      ADD(double_float,  exact_integer), ADD(double_float,  ratio), ADD(double_float,  single_float), ADD(double_float,  double_float),
    };

    #undef ADD
  }

  auto operator +(const_reference x, const_reference y) -> value_type
  {
    return experimental::add.at(type_index<2>(x.type(), y.type()))(x, y);
  }
} // namespace kernel
} // namespace meevax
