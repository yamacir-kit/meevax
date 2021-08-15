/*
   Copyright 2018-2021 Tatsuya Yamasaki.

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
  auto operator * (exact_integer const& a, let const& b) -> let { return apply(mul, a, b); }
  auto operator + (exact_integer const& a, let const& b) -> let { return apply(add, a, b); }
  auto operator - (exact_integer const& a, let const& b) -> let { return apply      ([](auto&& a, auto&& b) { return a -  b; }, a, b); }
  auto operator / (exact_integer const& a, let const& b) -> let { return apply      ([](auto&& a, auto&& b) { return a /  b; }, a, b); }
  auto operator % (exact_integer const& a, let const& b) -> let { return apply      ([](auto&& a, auto&& b) { return a %  b; }, a, b); }
  auto operator !=(exact_integer const& a, let const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a != b; }, a, b); }
  auto operator < (exact_integer const& a, let const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a <  b; }, a, b); }
  auto operator <=(exact_integer const& a, let const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a <= b; }, a, b); }
  auto operator ==(exact_integer const& a, let const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a == b; }, a, b); }
  auto operator > (exact_integer const& a, let const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a >  b; }, a, b); }
  auto operator >=(exact_integer const& a, let const& b) -> bool   { return apply<bool>([](auto&& a, auto&& b) { return a >= b; }, a, b); }

  auto operator * (exact_integer const& a, exact_integer const& b) -> exact_integer { return exact_integer(mul, a, b); }
  auto operator + (exact_integer const& a, exact_integer const& b) -> exact_integer { return exact_integer(add, a, b); }
  auto operator - (exact_integer const& a, exact_integer const& b) -> exact_integer { return exact_integer(a, std::minus<void>(), b); }
  auto operator / (exact_integer const& a, exact_integer const& b) -> ratio         { return ratio(make(a), make(b)); }
  auto operator % (exact_integer const& a, exact_integer const& b) -> exact_integer { return exact_integer(a, std::modulus<void>(), b); }
  auto operator !=(exact_integer const& a, exact_integer const& b) -> boolean       { return mpz_cmp(a.value, b.value) != 0; }
  auto operator < (exact_integer const& a, exact_integer const& b) -> boolean       { return mpz_cmp(a.value, b.value) <  0; }
  auto operator <=(exact_integer const& a, exact_integer const& b) -> boolean       { return mpz_cmp(a.value, b.value) <= 0; }
  auto operator ==(exact_integer const& a, exact_integer const& b) -> boolean       { return mpz_cmp(a.value, b.value) == 0; }
  auto operator > (exact_integer const& a, exact_integer const& b) -> boolean       { return mpz_cmp(a.value, b.value) >  0; }
  auto operator >=(exact_integer const& a, exact_integer const& b) -> boolean       { return mpz_cmp(a.value, b.value) >= 0; }

  auto operator * (exact_integer const& a, ratio const& b) -> ratio { return ratio(make(a * b.numerator()), cdr(b)); }
  auto operator + (exact_integer const& a, ratio const& b) -> ratio { return ratio(make(a * b.denominator() + b.numerator()), cdr(b)); }
  auto operator - (exact_integer const& a, ratio const& b) -> ratio { return ratio(make(a * b.denominator() - b.numerator()), cdr(b)); }
  auto operator / (exact_integer const& a, ratio const& b) -> ratio { return a * b.invert(); }
  auto operator % (exact_integer const&  , ratio const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
  auto operator !=(exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a != x.numerator() : boolean(false); }
  auto operator < (exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a <  x.numerator() : boolean(false); }
  auto operator <=(exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a <= x.numerator() : boolean(false); }
  auto operator ==(exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a == x.numerator() : boolean(false); }
  auto operator > (exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a >  x.numerator() : boolean(false); }
  auto operator >=(exact_integer const& a, ratio const& b) -> boolean { auto const x = b.reduce(); return x.is_integer() ? a >= x.numerator() : boolean(false); }

  auto operator * (ratio const& a, let const& b) -> let  { return apply(mul, a, b); }
  auto operator + (ratio const& a, let const& b) -> let  { return apply(add, a, b); }
  auto operator - (ratio const& a, let const& b) -> let  { return apply         ([](auto&& a, auto&& b) { return a -  b; }, a, b); }
  auto operator / (ratio const& a, let const& b) -> let  { return apply         ([](auto&& a, auto&& b) { return a /  b; }, a, b); }
  auto operator % (ratio const& a, let const& b) -> let  { return apply         ([](auto&& a, auto&& b) { return a %  b; }, a, b); }
  auto operator !=(ratio const& a, let const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a != b; }, a, b); }
  auto operator < (ratio const& a, let const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a <  b; }, a, b); }
  auto operator <=(ratio const& a, let const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a <= b; }, a, b); }
  auto operator ==(ratio const& a, let const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a == b; }, a, b); }
  auto operator > (ratio const& a, let const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a >  b; }, a, b); }
  auto operator >=(ratio const& a, let const& b) -> boolean { return apply<boolean>([](auto&& a, auto&& b) { return a >= b; }, a, b); }

  auto operator * (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator() * b), cdr(a)); }
  auto operator + (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator() + a.denominator() * b), cdr(a)); }
  auto operator - (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator() - a.denominator() * b), cdr(a)); }
  auto operator / (ratio const& a, exact_integer const& b) -> ratio { return ratio(car(a), make(a.denominator() * b)); }
  auto operator % (ratio const&  , exact_integer const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
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
  auto operator % (ratio const&  , ratio const&  ) -> ratio   { throw error(make<string>("unsupported operation"), unit); }
  auto operator ==(ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) == (b.numerator() * a.denominator()); }
  auto operator !=(ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) != (b.numerator() * a.denominator()); }
  auto operator < (ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) <  (b.numerator() * a.denominator()); }
  auto operator <=(ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) <= (b.numerator() * a.denominator()); }
  auto operator > (ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) >  (b.numerator() * a.denominator()); }
  auto operator >=(ratio const& a, ratio const& b) -> boolean { return (a.numerator() * b.denominator()) >= (b.numerator() * a.denominator()); }
} // namespace kernel
} // namespace meevax
