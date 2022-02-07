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
  auto exact_integer::operator * (const_reference b) const -> object { return apply(mul, *this, b); }
  auto exact_integer::operator + (const_reference b) const -> object { return apply(add, *this, b); }
  auto exact_integer::operator - (const_reference b) const -> object { return apply(sub, *this, b); }
  auto exact_integer::operator / (const_reference b) const -> object { return apply(div, *this, b); }
  auto exact_integer::operator % (const_reference b) const -> object { return apply(mod, *this, b); }
  auto exact_integer::operator !=(const_reference b) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a != b; }, *this, b); }
  auto exact_integer::operator < (const_reference b) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a <  b; }, *this, b); }
  auto exact_integer::operator <=(const_reference b) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a <= b; }, *this, b); }
  auto exact_integer::operator ==(const_reference b) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a == b; }, *this, b); }
  auto exact_integer::operator > (const_reference b) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a >  b; }, *this, b); }
  auto exact_integer::operator >=(const_reference b) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a >= b; }, *this, b); }

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

  auto ratio::operator * (const_reference x) const -> object { return apply(mul, *this, x); }
  auto ratio::operator + (const_reference x) const -> object { return apply(add, *this, x); }
  auto ratio::operator - (const_reference x) const -> object { return apply(sub, *this, x); }
  auto ratio::operator / (const_reference x) const -> object { return apply(div, *this, x); }
  auto ratio::operator % (const_reference x) const -> object { return apply(mod, *this, x); }
  auto ratio::operator !=(const_reference x) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a != b; }, *this, x); }
  auto ratio::operator < (const_reference x) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a <  b; }, *this, x); }
  auto ratio::operator <=(const_reference x) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a <= b; }, *this, x); }
  auto ratio::operator ==(const_reference x) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a == b; }, *this, x); }
  auto ratio::operator > (const_reference x) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a >  b; }, *this, x); }
  auto ratio::operator >=(const_reference x) const -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a >= b; }, *this, x); }

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
} // namespace kernel
} // namespace meevax
