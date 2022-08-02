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
  auto operator !=(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return std::invoke(is_integer(), x) and a != x.numerator().as<exact_integer>(); }
  auto operator < (exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return std::invoke(is_integer(), x) and a <  x.numerator().as<exact_integer>(); }
  auto operator <=(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return std::invoke(is_integer(), x) and a <= x.numerator().as<exact_integer>(); }
  auto operator ==(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return std::invoke(is_integer(), x) and a == x.numerator().as<exact_integer>(); }
  auto operator > (exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return std::invoke(is_integer(), x) and a >  x.numerator().as<exact_integer>(); }
  auto operator >=(exact_integer const& a, ratio const& b) -> bool  { auto const x = b.reduce(); return std::invoke(is_integer(), x) and a >= x.numerator().as<exact_integer>(); }

  auto operator + (exact_integer const& a, float b) -> float { return inexact_cast(a) +  b; }
  auto operator - (exact_integer const& a, float b) -> float { return inexact_cast(a) -  b; }
  auto operator * (exact_integer const& a, float b) -> float { return inexact_cast(a) *  b; }
  auto operator / (exact_integer const& a, float b) -> float { return inexact_cast(a) /  b; }
  auto operator % (exact_integer const& a, float b) -> float { return std::remainder(inexact_cast(a), b); }
  auto operator ==(exact_integer const& a, float b) -> bool  { return std::invoke(arithmetic_equal_to(), inexact_cast(a), b); }
  auto operator !=(exact_integer const& a, float b) -> bool  { return not (a == b); }
  auto operator < (exact_integer const& a, float b) -> bool  { return inexact_cast(a) <  b; }
  auto operator <=(exact_integer const& a, float b) -> bool  { return inexact_cast(a) <= b; }
  auto operator > (exact_integer const& a, float b) -> bool  { return inexact_cast(a) >  b; }
  auto operator >=(exact_integer const& a, float b) -> bool  { return inexact_cast(a) >= b; }

  auto operator + (exact_integer const& a, double b) -> double { return inexact_cast(a) +  b; }
  auto operator - (exact_integer const& a, double b) -> double { return inexact_cast(a) -  b; }
  auto operator * (exact_integer const& a, double b) -> double { return inexact_cast(a) *  b; }
  auto operator / (exact_integer const& a, double b) -> double { return inexact_cast(a) /  b; }
  auto operator % (exact_integer const& a, double b) -> double { return std::remainder(inexact_cast(a), b); }
  auto operator ==(exact_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) == 0; }
  auto operator !=(exact_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) != 0; }
  auto operator < (exact_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) <  0; }
  auto operator <=(exact_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) <= 0; }
  auto operator > (exact_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) >  0; }
  auto operator >=(exact_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) >= 0; }

  auto operator * (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() * b), cdr(a)); }
  auto operator + (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() + a.denominator().as<exact_integer>() * b), cdr(a)); }
  auto operator - (ratio const& a, exact_integer const& b) -> ratio { return ratio(make(a.numerator().as<exact_integer>() - a.denominator().as<exact_integer>() * b), cdr(a)); }
  auto operator / (ratio const& a, exact_integer const& b) -> ratio { return ratio(car(a), make(a.denominator().as<exact_integer>() * b)); }
  auto operator % (ratio const&  , exact_integer const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
  auto operator !=(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return std::invoke(is_integer(), x) and x.numerator().as<exact_integer>() != b; }
  auto operator < (ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return std::invoke(is_integer(), x) and x.numerator().as<exact_integer>() <  b; }
  auto operator <=(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return std::invoke(is_integer(), x) and x.numerator().as<exact_integer>() <= b; }
  auto operator ==(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return std::invoke(is_integer(), x) and x.numerator().as<exact_integer>() == b; }
  auto operator > (ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return std::invoke(is_integer(), x) and x.numerator().as<exact_integer>() >  b; }
  auto operator >=(ratio const& a, exact_integer const& b) -> bool  { auto const x = a.reduce(); return std::invoke(is_integer(), x) and x.numerator().as<exact_integer>() >= b; }

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

  auto operator + (ratio const& a, float b) -> float { return inexact_cast(a) +  b; }
  auto operator - (ratio const& a, float b) -> float { return inexact_cast(a) -  b; }
  auto operator * (ratio const& a, float b) -> float { return inexact_cast(a) *  b; }
  auto operator / (ratio const& a, float b) -> float { return inexact_cast(a) /  b; }
  auto operator % (ratio const& a, float b) -> float { return std::remainder(inexact_cast(a), b); }
  auto operator ==(ratio const& a, float b) -> bool  { return std::invoke(arithmetic_equal_to(), inexact_cast(a), b); }
  auto operator !=(ratio const& a, float b) -> bool  { return not (a == b); }
  auto operator < (ratio const& a, float b) -> bool  { return inexact_cast(a) <  b; }
  auto operator <=(ratio const& a, float b) -> bool  { return inexact_cast(a) <= b; }
  auto operator > (ratio const& a, float b) -> bool  { return inexact_cast(a) >  b; }
  auto operator >=(ratio const& a, float b) -> bool  { return inexact_cast(a) >= b; }

  auto operator + (ratio const& a, double b) -> double { return inexact_cast(a) +  b; }
  auto operator - (ratio const& a, double b) -> double { return inexact_cast(a) -  b; }
  auto operator * (ratio const& a, double b) -> double { return inexact_cast(a) *  b; }
  auto operator / (ratio const& a, double b) -> double { return inexact_cast(a) /  b; }
  auto operator % (ratio const& a, double b) -> double { return std::remainder(inexact_cast(a), b); }
  auto operator ==(ratio const& a, double b) -> bool   { return std::invoke(arithmetic_equal_to(), inexact_cast(a), b); }
  auto operator !=(ratio const& a, double b) -> bool   { return not (a == b); }
  auto operator < (ratio const& a, double b) -> bool   { return inexact_cast(a) <  b; }
  auto operator <=(ratio const& a, double b) -> bool   { return inexact_cast(a) <= b; }
  auto operator > (ratio const& a, double b) -> bool   { return inexact_cast(a) >  b; }
  auto operator >=(ratio const& a, double b) -> bool   { return inexact_cast(a) >= b; }

  auto operator + (float a, exact_integer const& b) -> float { return a +  inexact_cast(b); }
  auto operator - (float a, exact_integer const& b) -> float { return a -  inexact_cast(b); }
  auto operator * (float a, exact_integer const& b) -> float { return a *  inexact_cast(b); }
  auto operator / (float a, exact_integer const& b) -> float { return a /  inexact_cast(b); }
  auto operator % (float a, exact_integer const& b) -> float { return std::remainder(a, inexact_cast(b)); }
  auto operator ==(float a, exact_integer const& b) -> bool  { return std::invoke(arithmetic_equal_to(), a, inexact_cast(b)); }
  auto operator !=(float a, exact_integer const& b) -> bool  { return not (a == b); }
  auto operator < (float a, exact_integer const& b) -> bool  { return a <  inexact_cast(b); }
  auto operator <=(float a, exact_integer const& b) -> bool  { return a <= inexact_cast(b); }
  auto operator > (float a, exact_integer const& b) -> bool  { return a >  inexact_cast(b); }
  auto operator >=(float a, exact_integer const& b) -> bool  { return a >= inexact_cast(b); }

  auto operator + (float a, ratio const& b) -> float { return a +  inexact_cast(b); }
  auto operator - (float a, ratio const& b) -> float { return a -  inexact_cast(b); }
  auto operator * (float a, ratio const& b) -> float { return a *  inexact_cast(b); }
  auto operator / (float a, ratio const& b) -> float { return a /  inexact_cast(b); }
  auto operator % (float a, ratio const& b) -> float { return std::remainder(a, inexact_cast(b)); }
  auto operator ==(float a, ratio const& b) -> bool  { return std::invoke(arithmetic_equal_to(), a, inexact_cast(b)); }
  auto operator !=(float a, ratio const& b) -> bool  { return not (a == b); }
  auto operator < (float a, ratio const& b) -> bool  { return a <  inexact_cast(b); }
  auto operator <=(float a, ratio const& b) -> bool  { return a <= inexact_cast(b); }
  auto operator > (float a, ratio const& b) -> bool  { return a >  inexact_cast(b); }
  auto operator >=(float a, ratio const& b) -> bool  { return a >= inexact_cast(b); }

  auto operator + (double a, exact_integer const& b) -> double { return a +  inexact_cast(b); }
  auto operator - (double a, exact_integer const& b) -> double { return a -  inexact_cast(b); }
  auto operator * (double a, exact_integer const& b) -> double { return a *  inexact_cast(b); }
  auto operator / (double a, exact_integer const& b) -> double { return a /  inexact_cast(b); }
  auto operator % (double a, exact_integer const& b) -> double { return std::remainder(a, inexact_cast(b)); }
  auto operator ==(double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) == 0; }
  auto operator !=(double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) != 0; }
  auto operator < (double a, exact_integer const& b) -> bool   { return a <  inexact_cast(b); }
  auto operator <=(double a, exact_integer const& b) -> bool   { return a <= inexact_cast(b); }
  auto operator > (double a, exact_integer const& b) -> bool   { return a >  inexact_cast(b); }
  auto operator >=(double a, exact_integer const& b) -> bool   { return a >= inexact_cast(b); }

  auto operator + (double a, ratio const& b) -> double { return a +  inexact_cast(b); }
  auto operator - (double a, ratio const& b) -> double { return a -  inexact_cast(b); }
  auto operator * (double a, ratio const& b) -> double { return a *  inexact_cast(b); }
  auto operator / (double a, ratio const& b) -> double { return a /  inexact_cast(b); }
  auto operator % (double a, ratio const& b) -> double { return std::remainder(a, inexact_cast(b)); }
  auto operator ==(double a, ratio const& b) -> bool   { return std::invoke(arithmetic_equal_to(), a, inexact_cast(b)); }
  auto operator !=(double a, ratio const& b) -> bool   { return not (a == b); }
  auto operator < (double a, ratio const& b) -> bool   { return a <  inexact_cast(b); }
  auto operator <=(double a, ratio const& b) -> bool   { return a <= inexact_cast(b); }
  auto operator > (double a, ratio const& b) -> bool   { return a >  inexact_cast(b); }
  auto operator >=(double a, ratio const& b) -> bool   { return a >= inexact_cast(b); }

  auto operator +(const_reference x, const_reference y) -> value_type { return apply<std::plus      <void>>(x, y); }
  auto operator -(const_reference x, const_reference y) -> value_type { return apply<std::minus     <void>>(x, y); }
  auto operator *(const_reference x, const_reference y) -> value_type { return apply<std::multiplies<void>>(x, y); }
  auto operator /(const_reference x, const_reference y) -> value_type { return apply<std::divides   <void>>(x, y); }
  auto operator %(const_reference x, const_reference y) -> value_type { return apply<     modulus         >(x, y); }

  auto exact_integer_sqrt(exact_integer const& x) -> std::tuple<exact_integer, exact_integer>
  {
    exact_integer s, r;
    mpz_rootrem(s.value, r.value, x.value, 2);
    return std::make_tuple(s, r);
  }
} // namespace kernel
} // namespace meevax
