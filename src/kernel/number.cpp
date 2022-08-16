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
  auto operator + (exact_integer const& a, exact_integer const& b) -> exact_integer { exact_integer n; mpz_add(n.value, a.value, b.value); return n; }
  auto operator - (exact_integer const& a, exact_integer const& b) -> exact_integer { exact_integer n; mpz_sub(n.value, a.value, b.value); return n; }
  auto operator * (exact_integer const& a, exact_integer const& b) -> exact_integer { exact_integer n; mpz_mul(n.value, a.value, b.value); return n; }
  auto operator / (exact_integer const& a, exact_integer const& b) -> ratio         { return ratio(a, b); }
  auto operator % (exact_integer const& a, exact_integer const& b) -> exact_integer { exact_integer n; mpz_tdiv_r(n.value, a.value, b.value); return n; }
  auto operator ==(exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) == 0; }
  auto operator !=(exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) != 0; }
  auto operator < (exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) <  0; }
  auto operator <=(exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) <= 0; }
  auto operator > (exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) >  0; }
  auto operator >=(exact_integer const& a, exact_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) >= 0; }

  auto operator + (exact_integer const& a, ratio const& b) -> ratio { ratio q; mpq_add(q.value, ratio(a).value, b.value); return q; }
  auto operator - (exact_integer const& a, ratio const& b) -> ratio { ratio q; mpq_sub(q.value, ratio(a).value, b.value); return q; }
  auto operator * (exact_integer const& a, ratio const& b) -> ratio { ratio q; mpq_mul(q.value, ratio(a).value, b.value); return q; }
  auto operator / (exact_integer const& a, ratio const& b) -> ratio { ratio q; mpq_div(q.value, ratio(a).value, b.value); return q; }
  auto operator % (exact_integer const&  , ratio const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
  auto operator ==(exact_integer const& a, ratio const& b) -> bool  { return mpq_cmp_z(b.value, a.value) == 0; }
  auto operator !=(exact_integer const& a, ratio const& b) -> bool  { return mpq_cmp_z(b.value, a.value) != 0; }
  auto operator < (exact_integer const& a, ratio const& b) -> bool  { return mpq_cmp_z(b.value, a.value) >  0; }
  auto operator <=(exact_integer const& a, ratio const& b) -> bool  { return mpq_cmp_z(b.value, a.value) >= 0; }
  auto operator > (exact_integer const& a, ratio const& b) -> bool  { return mpq_cmp_z(b.value, a.value) <  0; }
  auto operator >=(exact_integer const& a, ratio const& b) -> bool  { return mpq_cmp_z(b.value, a.value) <= 0; }

  auto operator + (exact_integer const& a, float b) -> float { return inexact_cast(a) +  b; }
  auto operator - (exact_integer const& a, float b) -> float { return inexact_cast(a) -  b; }
  auto operator * (exact_integer const& a, float b) -> float { return inexact_cast(a) *  b; }
  auto operator / (exact_integer const& a, float b) -> float { return inexact_cast(a) /  b; }
  auto operator % (exact_integer const& a, float b) -> float { return std::remainder(inexact_cast(a), b); }
  auto operator ==(exact_integer const& a, float b) -> bool  { return std::invoke(equal_to(), inexact_cast(a), b); }
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

  auto operator + (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_add(q.value, a.value, ratio(b).value); return q; }
  auto operator - (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_sub(q.value, a.value, ratio(b).value); return q; }
  auto operator * (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_mul(q.value, a.value, ratio(b).value); return q; }
  auto operator / (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_div(q.value, a.value, ratio(b).value); return q; }
  auto operator % (ratio const&  , exact_integer const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
  auto operator ==(ratio const& a, exact_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) == 0; }
  auto operator !=(ratio const& a, exact_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) != 0; }
  auto operator < (ratio const& a, exact_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) <  0; }
  auto operator <=(ratio const& a, exact_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) <= 0; }
  auto operator > (ratio const& a, exact_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) >  0; }
  auto operator >=(ratio const& a, exact_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) >= 0; }

  auto operator + (ratio const& a, ratio const& b) -> ratio { ratio q; mpq_add(q.value, a.value, b.value); return q; }
  auto operator - (ratio const& a, ratio const& b) -> ratio { ratio q; mpq_sub(q.value, a.value, b.value); return q; }
  auto operator * (ratio const& a, ratio const& b) -> ratio { ratio q; mpq_mul(q.value, a.value, b.value); return q; }
  auto operator / (ratio const& a, ratio const& b) -> ratio { ratio q; mpq_div(q.value, a.value, b.value); return q; }
  auto operator % (ratio const&  , ratio const&  ) -> ratio { throw error(make<string>("unsupported operation"), unit); }
  auto operator ==(ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) == 0; }
  auto operator !=(ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) != 0; }
  auto operator < (ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) <  0; }
  auto operator <=(ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) <= 0; }
  auto operator > (ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) >  0; }
  auto operator >=(ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) >= 0; }

  auto operator + (ratio const& a, float b) -> float { return inexact_cast(a) +  b; }
  auto operator - (ratio const& a, float b) -> float { return inexact_cast(a) -  b; }
  auto operator * (ratio const& a, float b) -> float { return inexact_cast(a) *  b; }
  auto operator / (ratio const& a, float b) -> float { return inexact_cast(a) /  b; }
  auto operator % (ratio const& a, float b) -> float { return std::remainder(inexact_cast(a), b); }
  auto operator ==(ratio const& a, float b) -> bool  { return std::invoke(equal_to(), inexact_cast(a), b); }
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
  auto operator ==(ratio const& a, double b) -> bool   { return std::invoke(equal_to(), inexact_cast(a), b); }
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
  auto operator ==(float a, exact_integer const& b) -> bool  { return std::invoke(equal_to(), a, inexact_cast(b)); }
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
  auto operator ==(float a, ratio const& b) -> bool  { return std::invoke(equal_to(), a, inexact_cast(b)); }
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
  auto operator < (double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) >  0; }
  auto operator <=(double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) >= 0; }
  auto operator > (double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) <  0; }
  auto operator >=(double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) <= 0; }

  auto operator + (double a, ratio const& b) -> double { return a +  inexact_cast(b); }
  auto operator - (double a, ratio const& b) -> double { return a -  inexact_cast(b); }
  auto operator * (double a, ratio const& b) -> double { return a *  inexact_cast(b); }
  auto operator / (double a, ratio const& b) -> double { return a /  inexact_cast(b); }
  auto operator % (double a, ratio const& b) -> double { return std::remainder(a, inexact_cast(b)); }
  auto operator ==(double a, ratio const& b) -> bool   { return std::invoke(equal_to(), a, inexact_cast(b)); }
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
} // namespace kernel
} // namespace meevax
