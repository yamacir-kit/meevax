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
  auto operator !=(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return is_integer()(x) ? a != x.numerator().as<exact_integer>() : false; }
  auto operator < (exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return is_integer()(x) ? a <  x.numerator().as<exact_integer>() : false; }
  auto operator <=(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return is_integer()(x) ? a <= x.numerator().as<exact_integer>() : false; }
  auto operator ==(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return is_integer()(x) ? a == x.numerator().as<exact_integer>() : false; }
  auto operator > (exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return is_integer()(x) ? a >  x.numerator().as<exact_integer>() : false; }
  auto operator >=(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return is_integer()(x) ? a >= x.numerator().as<exact_integer>() : false; }

  auto operator * (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() * b), cdr(a)); }
  auto operator + (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() + a.denominator().as<exact_integer>() * b), cdr(a)); }
  auto operator - (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() - a.denominator().as<exact_integer>() * b), cdr(a)); }
  auto operator / (ratio const& a, exact_integer const& b) -> ratio { return ratio(car(a), make(a.denominator().as<exact_integer>() * b)); }
  auto operator % (ratio const&  , exact_integer const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
  auto operator !=(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return is_integer()(x) ? x.numerator().as<exact_integer>() != b : false; }
  auto operator < (ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return is_integer()(x) ? x.numerator().as<exact_integer>() <  b : false; }
  auto operator <=(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return is_integer()(x) ? x.numerator().as<exact_integer>() <= b : false; }
  auto operator ==(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return is_integer()(x) ? x.numerator().as<exact_integer>() == b : false; }
  auto operator > (ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return is_integer()(x) ? x.numerator().as<exact_integer>() >  b : false; }
  auto operator >=(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return is_integer()(x) ? x.numerator().as<exact_integer>() >= b : false; }

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

  auto operator +(const_reference x, const_reference y) -> value_type { return apply<std::plus      <void>>(x, y); }
  auto operator -(const_reference x, const_reference y) -> value_type { return apply<std::minus     <void>>(x, y); }
  auto operator *(const_reference x, const_reference y) -> value_type { return apply<std::multiplies<void>>(x, y); }
  auto operator /(const_reference x, const_reference y) -> value_type { return apply<std::divides   <void>>(x, y); }
  auto operator %(const_reference x, const_reference y) -> value_type { return apply<std::modulus   <void>>(x, y); }

  auto exact_integer_sqrt(exact_integer const& x) -> std::tuple<exact_integer, exact_integer>
  {
    exact_integer s, r;
    mpz_rootrem(s.value, r.value, x.value, 2);
    return std::make_tuple(s, r);
  }
} // namespace kernel
} // namespace meevax
