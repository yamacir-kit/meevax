/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <charconv>
#include <memory> // std::unique_ptr
#include <regex>
#include <string_view>

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/number/nearest_integer.hpp>
#include <meevax/kernel/number/power.hpp>
#include <meevax/kernel/number/trigonometric.hpp>
#include <meevax/kernel/string.hpp>

namespace meevax::inline kernel
{
  auto operator * (widen_integer a, large_integer const& b) -> large_integer { return large_integer(a) * b; }
  auto operator + (widen_integer a, large_integer const& b) -> large_integer { return large_integer(a) + b; }
  auto operator - (widen_integer a, large_integer const& b) -> large_integer { return large_integer(a) - b; }
  auto operator / (widen_integer a, large_integer const& b) -> ratio         { return large_integer(a) / b; }
  auto operator % (widen_integer a, large_integer const& b) -> large_integer { return large_integer(a) % b; }
  auto operator ==(widen_integer a, large_integer const& b) -> bool          { return 0 == mpz_cmp_si(b.value, a); }
  auto operator !=(widen_integer a, large_integer const& b) -> bool          { return 0 != mpz_cmp_si(b.value, a); }
  auto operator < (widen_integer a, large_integer const& b) -> bool          { return 0 <  mpz_cmp_si(b.value, a); }
  auto operator <=(widen_integer a, large_integer const& b) -> bool          { return 0 <= mpz_cmp_si(b.value, a); }
  auto operator > (widen_integer a, large_integer const& b) -> bool          { return 0 >  mpz_cmp_si(b.value, a); }
  auto operator >=(widen_integer a, large_integer const& b) -> bool          { return 0 >= mpz_cmp_si(b.value, a); }

  auto operator + (widen_integer a, ratio const& b) -> ratio { ratio q; mpq_add(q.value, ratio(a).value, b.value); return q; }
  auto operator - (widen_integer a, ratio const& b) -> ratio { ratio q; mpq_sub(q.value, ratio(a).value, b.value); return q; }
  auto operator * (widen_integer a, ratio const& b) -> ratio { ratio q; mpq_mul(q.value, ratio(a).value, b.value); return q; }
  auto operator / (widen_integer a, ratio const& b) -> ratio { ratio q; mpq_div(q.value, ratio(a).value, b.value); return q; }
  auto operator % (widen_integer  , ratio const&  ) -> ratio { throw std::runtime_error("unimplemented operation"); }
  auto operator ==(widen_integer a, ratio const& b) -> bool  { return 0 == mpq_cmp_si(b.value, a, 1); }
  auto operator !=(widen_integer a, ratio const& b) -> bool  { return 0 != mpq_cmp_si(b.value, a, 1); }
  auto operator < (widen_integer a, ratio const& b) -> bool  { return 0 <  mpq_cmp_si(b.value, a, 1); }
  auto operator <=(widen_integer a, ratio const& b) -> bool  { return 0 <= mpq_cmp_si(b.value, a, 1); }
  auto operator > (widen_integer a, ratio const& b) -> bool  { return 0 >  mpq_cmp_si(b.value, a, 1); }
  auto operator >=(widen_integer a, ratio const& b) -> bool  { return 0 >= mpq_cmp_si(b.value, a, 1); }

  auto operator + (widen_integer a, complex const& b) -> complex { return complex(make(static_cast<small_integer>(a)), e0) +  b; }
  auto operator - (widen_integer a, complex const& b) -> complex { return complex(make(static_cast<small_integer>(a)), e0) -  b; }
  auto operator * (widen_integer a, complex const& b) -> complex { return complex(make(static_cast<small_integer>(a)), e0) *  b; }
  auto operator / (widen_integer a, complex const& b) -> complex { return complex(make(static_cast<small_integer>(a)), e0) /  b; }
  auto operator ==(widen_integer a, complex const& b) -> bool    { return complex(make(static_cast<small_integer>(a)), e0) == b; }
  auto operator !=(widen_integer a, complex const& b) -> bool    { return complex(make(static_cast<small_integer>(a)), e0) != b; }

  auto operator + (large_integer const& a, widen_integer b) -> large_integer { return a + large_integer(b); }
  auto operator - (large_integer const& a, widen_integer b) -> large_integer { return a - large_integer(b); }
  auto operator * (large_integer const& a, widen_integer b) -> large_integer { return a * large_integer(b); }
  auto operator / (large_integer const& a, widen_integer b) -> ratio         { return a / large_integer(b); }
  auto operator % (large_integer const& a, widen_integer b) -> large_integer { return a % large_integer(b); }
  auto operator ==(large_integer const& a, widen_integer b) -> bool          { return mpz_cmp_si(a.value, b) == 0; }
  auto operator !=(large_integer const& a, widen_integer b) -> bool          { return mpz_cmp_si(a.value, b) != 0; }
  auto operator < (large_integer const& a, widen_integer b) -> bool          { return mpz_cmp_si(a.value, b) <  0; }
  auto operator <=(large_integer const& a, widen_integer b) -> bool          { return mpz_cmp_si(a.value, b) <= 0; }
  auto operator > (large_integer const& a, widen_integer b) -> bool          { return mpz_cmp_si(a.value, b) >  0; }
  auto operator >=(large_integer const& a, widen_integer b) -> bool          { return mpz_cmp_si(a.value, b) >= 0; }

  auto operator + (large_integer const& a, large_integer const& b) -> large_integer { large_integer n; mpz_add(n.value, a.value, b.value); return n; }
  auto operator - (large_integer const& a, large_integer const& b) -> large_integer { large_integer n; mpz_sub(n.value, a.value, b.value); return n; }
  auto operator * (large_integer const& a, large_integer const& b) -> large_integer { large_integer n; mpz_mul(n.value, a.value, b.value); return n; }
  auto operator / (large_integer const& a, large_integer const& b) -> ratio         { return ratio(a, b); }
  auto operator % (large_integer const& a, large_integer const& b) -> large_integer { large_integer n; mpz_tdiv_r(n.value, a.value, b.value); return n; }
  auto operator ==(large_integer const& a, large_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) == 0; }
  auto operator !=(large_integer const& a, large_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) != 0; }
  auto operator < (large_integer const& a, large_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) <  0; }
  auto operator <=(large_integer const& a, large_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) <= 0; }
  auto operator > (large_integer const& a, large_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) >  0; }
  auto operator >=(large_integer const& a, large_integer const& b) -> bool          { return mpz_cmp(a.value, b.value) >= 0; }

  auto operator + (large_integer const& a, ratio const& b) -> ratio { ratio q; mpq_add(q.value, ratio(a).value, b.value); return q; }
  auto operator - (large_integer const& a, ratio const& b) -> ratio { ratio q; mpq_sub(q.value, ratio(a).value, b.value); return q; }
  auto operator * (large_integer const& a, ratio const& b) -> ratio { ratio q; mpq_mul(q.value, ratio(a).value, b.value); return q; }
  auto operator / (large_integer const& a, ratio const& b) -> ratio { ratio q; mpq_div(q.value, ratio(a).value, b.value); return q; }
  auto operator % (large_integer const&  , ratio const&  ) -> ratio { throw std::invalid_argument("unimplemented operation"); }
  auto operator ==(large_integer const& a, ratio const& b) -> bool  { return 0 == mpq_cmp_z(b.value, a.value); }
  auto operator !=(large_integer const& a, ratio const& b) -> bool  { return 0 != mpq_cmp_z(b.value, a.value); }
  auto operator < (large_integer const& a, ratio const& b) -> bool  { return 0 <  mpq_cmp_z(b.value, a.value); }
  auto operator <=(large_integer const& a, ratio const& b) -> bool  { return 0 <= mpq_cmp_z(b.value, a.value); }
  auto operator > (large_integer const& a, ratio const& b) -> bool  { return 0 >  mpq_cmp_z(b.value, a.value); }
  auto operator >=(large_integer const& a, ratio const& b) -> bool  { return 0 >= mpq_cmp_z(b.value, a.value); }

  auto operator + (large_integer const& a, float b) -> float { return static_cast<float>(a) +  b; }
  auto operator - (large_integer const& a, float b) -> float { return static_cast<float>(a) -  b; }
  auto operator * (large_integer const& a, float b) -> float { return static_cast<float>(a) *  b; }
  auto operator / (large_integer const& a, float b) -> float { return static_cast<float>(a) /  b; }
  auto operator % (large_integer const& a, float b) -> float { return std::remainder(static_cast<float>(a), b); }
  auto operator ==(large_integer const& a, float b) -> bool  { return mpz_cmp_d(a.value, b) == 0; }
  auto operator !=(large_integer const& a, float b) -> bool  { return mpz_cmp_d(a.value, b) != 0; }
  auto operator < (large_integer const& a, float b) -> bool  { return mpz_cmp_d(a.value, b) <  0; }
  auto operator <=(large_integer const& a, float b) -> bool  { return mpz_cmp_d(a.value, b) <= 0; }
  auto operator > (large_integer const& a, float b) -> bool  { return mpz_cmp_d(a.value, b) >  0; }
  auto operator >=(large_integer const& a, float b) -> bool  { return mpz_cmp_d(a.value, b) >= 0; }

  auto operator + (large_integer const& a, double b) -> double { return static_cast<double>(a) +  b; }
  auto operator - (large_integer const& a, double b) -> double { return static_cast<double>(a) -  b; }
  auto operator * (large_integer const& a, double b) -> double { return static_cast<double>(a) *  b; }
  auto operator / (large_integer const& a, double b) -> double { return static_cast<double>(a) /  b; }
  auto operator % (large_integer const& a, double b) -> double { return std::remainder(static_cast<double>(a), b); }
  auto operator ==(large_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) == 0; }
  auto operator !=(large_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) != 0; }
  auto operator < (large_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) <  0; }
  auto operator <=(large_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) <= 0; }
  auto operator > (large_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) >  0; }
  auto operator >=(large_integer const& a, double b) -> bool   { return mpz_cmp_d(a.value, b) >= 0; }

  auto operator + (large_integer const& a, complex const& b) -> complex { return complex(make(a), e0) +  b; }
  auto operator - (large_integer const& a, complex const& b) -> complex { return complex(make(a), e0) -  b; }
  auto operator * (large_integer const& a, complex const& b) -> complex { return complex(make(a), e0) *  b; }
  auto operator / (large_integer const& a, complex const& b) -> complex { return complex(make(a), e0) /  b; }
  auto operator ==(large_integer const& a, complex const& b) -> bool    { return complex(make(a), e0) == b; }
  auto operator !=(large_integer const& a, complex const& b) -> bool    { return complex(make(a), e0) != b; }

  auto operator + (ratio const& a, widen_integer b) -> ratio { ratio q; mpq_add(q.value, a.value, ratio(b).value); return q; }
  auto operator - (ratio const& a, widen_integer b) -> ratio { ratio q; mpq_sub(q.value, a.value, ratio(b).value); return q; }
  auto operator * (ratio const& a, widen_integer b) -> ratio { ratio q; mpq_mul(q.value, a.value, ratio(b).value); return q; }
  auto operator / (ratio const& a, widen_integer b) -> ratio { ratio q; mpq_div(q.value, a.value, ratio(b).value); return q; }
  auto operator % (ratio const&  , widen_integer  ) -> ratio { throw std::invalid_argument("unimplemented operation"); }
  auto operator ==(ratio const& a, widen_integer b) -> bool  { return mpq_cmp_si(a.value, b, 1) == 0; }
  auto operator !=(ratio const& a, widen_integer b) -> bool  { return mpq_cmp_si(a.value, b, 1) != 0; }
  auto operator < (ratio const& a, widen_integer b) -> bool  { return mpq_cmp_si(a.value, b, 1) <  0; }
  auto operator <=(ratio const& a, widen_integer b) -> bool  { return mpq_cmp_si(a.value, b, 1) <= 0; }
  auto operator > (ratio const& a, widen_integer b) -> bool  { return mpq_cmp_si(a.value, b, 1) >  0; }
  auto operator >=(ratio const& a, widen_integer b) -> bool  { return mpq_cmp_si(a.value, b, 1) >= 0; }

  auto operator + (ratio const& a, large_integer const& b) -> ratio { ratio q; mpq_add(q.value, a.value, ratio(b).value); return q; }
  auto operator - (ratio const& a, large_integer const& b) -> ratio { ratio q; mpq_sub(q.value, a.value, ratio(b).value); return q; }
  auto operator * (ratio const& a, large_integer const& b) -> ratio { ratio q; mpq_mul(q.value, a.value, ratio(b).value); return q; }
  auto operator / (ratio const& a, large_integer const& b) -> ratio { ratio q; mpq_div(q.value, a.value, ratio(b).value); return q; }
  auto operator % (ratio const&  , large_integer const&  ) -> ratio { throw std::invalid_argument("unimplemented operation"); }
  auto operator ==(ratio const& a, large_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) == 0; }
  auto operator !=(ratio const& a, large_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) != 0; }
  auto operator < (ratio const& a, large_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) <  0; }
  auto operator <=(ratio const& a, large_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) <= 0; }
  auto operator > (ratio const& a, large_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) >  0; }
  auto operator >=(ratio const& a, large_integer const& b) -> bool  { return mpq_cmp_z(a.value, b.value) >= 0; }

  auto operator + (ratio const& a, ratio const& b) -> ratio { ratio q; mpq_add(q.value, a.value, b.value); return q; }
  auto operator - (ratio const& a, ratio const& b) -> ratio { ratio q; mpq_sub(q.value, a.value, b.value); return q; }
  auto operator * (ratio const& a, ratio const& b) -> ratio { ratio q; mpq_mul(q.value, a.value, b.value); return q; }
  auto operator / (ratio const& a, ratio const& b) -> ratio { ratio q; mpq_div(q.value, a.value, b.value); return q; }
  auto operator % (ratio const&  , ratio const&  ) -> ratio { throw std::invalid_argument("unimplemented operation"); }
  auto operator ==(ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) == 0; }
  auto operator !=(ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) != 0; }
  auto operator < (ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) <  0; }
  auto operator <=(ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) <= 0; }
  auto operator > (ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) >  0; }
  auto operator >=(ratio const& a, ratio const& b) -> bool  { return mpq_cmp(a.value, b.value) >= 0; }

  auto operator + (ratio const& a, float b) -> float { return static_cast<float>(a) +  b; }
  auto operator - (ratio const& a, float b) -> float { return static_cast<float>(a) -  b; }
  auto operator * (ratio const& a, float b) -> float { return static_cast<float>(a) *  b; }
  auto operator / (ratio const& a, float b) -> float { return static_cast<float>(a) /  b; }
  auto operator % (ratio const& a, float b) -> float { return std::remainder(static_cast<float>(a), b); }
  auto operator ==(ratio const& a, float b) -> bool  { return inexact_equals(static_cast<float>(a), b); }
  auto operator !=(ratio const& a, float b) -> bool  { return not (a == b); }
  auto operator < (ratio const& a, float b) -> bool  { return static_cast<float>(a) <  b; }
  auto operator <=(ratio const& a, float b) -> bool  { return static_cast<float>(a) <= b; }
  auto operator > (ratio const& a, float b) -> bool  { return static_cast<float>(a) >  b; }
  auto operator >=(ratio const& a, float b) -> bool  { return static_cast<float>(a) >= b; }

  auto operator + (ratio const& a, double b) -> double { return static_cast<double>(a) +  b; }
  auto operator - (ratio const& a, double b) -> double { return static_cast<double>(a) -  b; }
  auto operator * (ratio const& a, double b) -> double { return static_cast<double>(a) *  b; }
  auto operator / (ratio const& a, double b) -> double { return static_cast<double>(a) /  b; }
  auto operator % (ratio const& a, double b) -> double { return std::remainder(static_cast<double>(a), b); }
  auto operator ==(ratio const& a, double b) -> bool   { return inexact_equals(static_cast<double>(a), b); }
  auto operator !=(ratio const& a, double b) -> bool   { return not (a == b); }
  auto operator < (ratio const& a, double b) -> bool   { return static_cast<double>(a) <  b; }
  auto operator <=(ratio const& a, double b) -> bool   { return static_cast<double>(a) <= b; }
  auto operator > (ratio const& a, double b) -> bool   { return static_cast<double>(a) >  b; }
  auto operator >=(ratio const& a, double b) -> bool   { return static_cast<double>(a) >= b; }

  auto operator + (ratio const& a, complex const& b) -> complex { return complex(make(a), e0) +  b; }
  auto operator - (ratio const& a, complex const& b) -> complex { return complex(make(a), e0) -  b; }
  auto operator * (ratio const& a, complex const& b) -> complex { return complex(make(a), e0) *  b; }
  auto operator / (ratio const& a, complex const& b) -> complex { return complex(make(a), e0) /  b; }
  auto operator ==(ratio const& a, complex const& b) -> bool    { return complex(make(a), e0) == b; }
  auto operator !=(ratio const& a, complex const& b) -> bool    { return complex(make(a), e0) != b; }

  auto operator + (float a, large_integer const& b) -> float { return a +  static_cast<float>(b); }
  auto operator - (float a, large_integer const& b) -> float { return a -  static_cast<float>(b); }
  auto operator * (float a, large_integer const& b) -> float { return a *  static_cast<float>(b); }
  auto operator / (float a, large_integer const& b) -> float { return a /  static_cast<float>(b); }
  auto operator % (float a, large_integer const& b) -> float { return std::remainder(a, static_cast<float>(b)); }
  auto operator ==(float a, large_integer const& b) -> bool  { return 0 == mpz_cmp_d(b.value, a); }
  auto operator !=(float a, large_integer const& b) -> bool  { return 0 != mpz_cmp_d(b.value, a); }
  auto operator < (float a, large_integer const& b) -> bool  { return 0 <  mpz_cmp_d(b.value, a); }
  auto operator <=(float a, large_integer const& b) -> bool  { return 0 <= mpz_cmp_d(b.value, a); }
  auto operator > (float a, large_integer const& b) -> bool  { return 0 >  mpz_cmp_d(b.value, a); }
  auto operator >=(float a, large_integer const& b) -> bool  { return 0 >= mpz_cmp_d(b.value, a); }

  auto operator + (float a, ratio const& b) -> float { return a +  static_cast<float>(b); }
  auto operator - (float a, ratio const& b) -> float { return a -  static_cast<float>(b); }
  auto operator * (float a, ratio const& b) -> float { return a *  static_cast<float>(b); }
  auto operator / (float a, ratio const& b) -> float { return a /  static_cast<float>(b); }
  auto operator % (float a, ratio const& b) -> float { return std::remainder(a, static_cast<float>(b)); }
  auto operator ==(float a, ratio const& b) -> bool  { return inexact_equals(a, static_cast<float>(b)); }
  auto operator !=(float a, ratio const& b) -> bool  { return not (a == b); }
  auto operator < (float a, ratio const& b) -> bool  { return a <  static_cast<float>(b); }
  auto operator <=(float a, ratio const& b) -> bool  { return a <= static_cast<float>(b); }
  auto operator > (float a, ratio const& b) -> bool  { return a >  static_cast<float>(b); }
  auto operator >=(float a, ratio const& b) -> bool  { return a >= static_cast<float>(b); }

  auto operator + (float a, complex const& b) -> complex { return complex(make(a), e0) +  b; }
  auto operator - (float a, complex const& b) -> complex { return complex(make(a), e0) -  b; }
  auto operator * (float a, complex const& b) -> complex { return complex(make(a), e0) *  b; }
  auto operator / (float a, complex const& b) -> complex { return complex(make(a), e0) /  b; }
  auto operator ==(float a, complex const& b) -> bool    { return complex(make(a), e0) == b; }
  auto operator !=(float a, complex const& b) -> bool    { return complex(make(a), e0) != b; }

  auto operator + (double a, large_integer const& b) -> double { return a + static_cast<double>(b); }
  auto operator - (double a, large_integer const& b) -> double { return a - static_cast<double>(b); }
  auto operator * (double a, large_integer const& b) -> double { return a * static_cast<double>(b); }
  auto operator / (double a, large_integer const& b) -> double { return a / static_cast<double>(b); }
  auto operator % (double a, large_integer const& b) -> double { return std::remainder(a, static_cast<double>(b)); }
  auto operator ==(double a, large_integer const& b) -> bool   { return 0 == mpz_cmp_d(b.value, a); }
  auto operator !=(double a, large_integer const& b) -> bool   { return 0 != mpz_cmp_d(b.value, a); }
  auto operator < (double a, large_integer const& b) -> bool   { return 0 <  mpz_cmp_d(b.value, a); }
  auto operator <=(double a, large_integer const& b) -> bool   { return 0 <= mpz_cmp_d(b.value, a); }
  auto operator > (double a, large_integer const& b) -> bool   { return 0 >  mpz_cmp_d(b.value, a); }
  auto operator >=(double a, large_integer const& b) -> bool   { return 0 >= mpz_cmp_d(b.value, a); }

  auto operator + (double a, ratio const& b) -> double { return a +  static_cast<double>(b); }
  auto operator - (double a, ratio const& b) -> double { return a -  static_cast<double>(b); }
  auto operator * (double a, ratio const& b) -> double { return a *  static_cast<double>(b); }
  auto operator / (double a, ratio const& b) -> double { return a /  static_cast<double>(b); }
  auto operator % (double a, ratio const& b) -> double { return std::remainder(a, static_cast<double>(b)); }
  auto operator ==(double a, ratio const& b) -> bool   { return inexact_equals(a, static_cast<double>(b)); }
  auto operator !=(double a, ratio const& b) -> bool   { return not (a == b); }
  auto operator < (double a, ratio const& b) -> bool   { return a <  static_cast<double>(b); }
  auto operator <=(double a, ratio const& b) -> bool   { return a <= static_cast<double>(b); }
  auto operator > (double a, ratio const& b) -> bool   { return a >  static_cast<double>(b); }
  auto operator >=(double a, ratio const& b) -> bool   { return a >= static_cast<double>(b); }

  auto operator + (double a, complex const& b) -> complex { return complex(make(a), e0) +  b; }
  auto operator - (double a, complex const& b) -> complex { return complex(make(a), e0) -  b; }
  auto operator * (double a, complex const& b) -> complex { return complex(make(a), e0) *  b; }
  auto operator / (double a, complex const& b) -> complex { return complex(make(a), e0) /  b; }
  auto operator ==(double a, complex const& b) -> bool    { return complex(make(a), e0) == b; }
  auto operator !=(double a, complex const& b) -> bool    { return complex(make(a), e0) != b; }

  auto operator + (complex const& a, widen_integer b) -> complex { return a +  complex(make(static_cast<small_integer>(b)), e0); }
  auto operator - (complex const& a, widen_integer b) -> complex { return a -  complex(make(static_cast<small_integer>(b)), e0); }
  auto operator * (complex const& a, widen_integer b) -> complex { return a *  complex(make(static_cast<small_integer>(b)), e0); }
  auto operator / (complex const& a, widen_integer b) -> complex { return a /  complex(make(static_cast<small_integer>(b)), e0); }
  auto operator ==(complex const& a, widen_integer b) -> bool    { return a == complex(make(static_cast<small_integer>(b)), e0); }
  auto operator !=(complex const& a, widen_integer b) -> bool    { return a != complex(make(static_cast<small_integer>(b)), e0); }

  auto operator + (complex const& a, large_integer const& b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, large_integer const& b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, large_integer const& b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, large_integer const& b) -> complex { return a /  complex(make(b), e0); }
  auto operator ==(complex const& a, large_integer const& b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, large_integer const& b) -> bool    { return a != complex(make(b), e0); }

  auto operator + (complex const& a, ratio const& b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, ratio const& b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, ratio const& b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, ratio const& b) -> complex { return a /  complex(make(b), e0); }
  auto operator ==(complex const& a, ratio const& b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, ratio const& b) -> bool    { return a != complex(make(b), e0); }

  auto operator + (complex const& a, float b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, float b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, float b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, float b) -> complex { return a /  complex(make(b), e0); }
  auto operator ==(complex const& a, float b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, float b) -> bool    { return a != complex(make(b), e0); }

  auto operator + (complex const& a, double b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, double b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, double b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, double b) -> complex { return a /  complex(make(b), e0); }
  auto operator ==(complex const& a, double b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, double b) -> bool    { return a != complex(make(b), e0); }

  auto operator + (complex const& a, complex const& b) -> complex { return complex(a.real() + b.real(), a.imag() + b.imag()); }
  auto operator - (complex const& a, complex const& b) -> complex { return complex(a.real() - b.real(), a.imag() - b.imag()); }
  auto operator * (complex const& a, complex const& b) -> complex { return complex(a.real() * b.real() - a.imag() * b.imag(), a.imag() * b.real() + a.real() * b.imag()); }
  auto operator / (complex const& a, complex const& b) -> complex { auto x = a.real() * b.real() + a.imag() * b.imag(); auto y = a.imag() * b.real() - a.real() * b.imag(); auto d = b.real() * b.real() + b.imag() * b.imag(); return complex(x / d, y / d); }
  auto operator ==(complex const& a, complex const& b) -> bool    { return number::equals(a.real(), b.real()) and number::equals(a.imag(), b.imag()); }
  auto operator !=(complex const& a, complex const& b) -> bool    { return not (a == b); }

  auto operator +(object const& x, object const& y) -> object
  {
    using namespace number;
    return apply_to<complex_numbers>(std::plus(), x, y);
  }

  auto operator -(object const& x, object const& y) -> object
  {
    using namespace number;
    return apply_to<complex_numbers>(std::minus(), x, y);
  }

  auto operator *(object const& x, object const& y) -> object
  {
    using namespace number;
    return apply_to<complex_numbers>(std::multiplies(), x, y);
  }

  auto operator /(object const& x, object const& y) -> object
  {
    using namespace number;

    auto f = []<typename T, typename U>(T const& x, U const& y)
    {
      if constexpr (std::is_same_v<T, widen_integer> and
                    std::is_same_v<U, widen_integer>)
      {
        return make<ratio>(large_integer(x),
                           large_integer(y));
      }
      else
      {
        return x / y;
      }
    };

    return apply_to<complex_numbers>(f, x, y);
  }

  auto operator % (object const& x, object const& y) -> object
  {
    using namespace number;

    auto f = []<typename T, typename U>(T const& x, U const& y)
    {
      if constexpr ((std::is_floating_point_v<T> and std::is_arithmetic_v<U>) or
                    (std::is_floating_point_v<U> and std::is_arithmetic_v<T>))
      {
        return std::fmod(x, y);
      }
      else
      {
        return x % y;
      }
    };

    return apply_to<real_numbers>(f, x, y);
  }

  auto make_integer(std::string const& literal, int radix) -> object
  {
    auto value = widen_integer(0);

    if (auto result = std::from_chars(literal.data(), literal.data() + literal.size(), value, radix); result.ec == std::errc())
    {
      if (result.ptr != literal.data() + literal.size())
      {
        throw std::invalid_argument("not an integer");
      }
      else if (std::numeric_limits<small_integer>::min() <= value and value <= std::numeric_limits<small_integer>::max())
      {
        return make(static_cast<small_integer>(value));
      }
      else
      {
        return make<large_integer>(literal, radix);
      }
    }
    else
    {
      switch (result.ec)
      {
      case std::errc::result_out_of_range:
        return make<large_integer>(literal, radix);

      default:
      case std::errc::invalid_argument:
        throw std::invalid_argument("not an integer");
      }
    }
  }

  auto make_rational(std::string const& literal, int radix) -> object
  {
    try
    {
      return make_integer(literal, radix);
    }
    catch (std::invalid_argument const&)
    {
      return make<ratio>(literal, radix);
    }
  }

  auto make_real(std::string const& literal, int radix) -> object
  {
    try
    {
      return make_rational(literal, radix);
    }
    catch (std::invalid_argument const&)
    {
      std::unordered_map<std::string_view, double> static const constants
      {
        // R7RS 7.1.1. Lexical structure
        { "+inf.0", +std::numeric_limits<double>::infinity()  },
        { "-inf.0", -std::numeric_limits<double>::infinity()  },
        { "+nan.0", +std::numeric_limits<double>::quiet_NaN() },
        { "-nan.0", -std::numeric_limits<double>::quiet_NaN() },
      };

      auto static const pattern = std::regex(R"([+-]?(?:\d+\.?|\d*\.\d+)(?:([DEFdef])[+-]?\d+)?)");

      if (auto iter = constants.find(literal); iter != constants.end())
      {
        return make(iter->second);
      }
      else if (auto result = std::smatch(); std::regex_match(literal, result, pattern))
      {
        /*
           R7RS 6.2.5. Syntax of numerical constants

           In systems with inexact numbers of varying precisions it can be
           useful to specify the precision of a constant. For this purpose,
           implementations may accept numerical constants written with an
           exponent marker that indicates the desired precision of the inexact
           representation. If so, the letter s, f, d, or l, meaning short,
           single, double, or long precision, respectively, can be used in
           place of e. The default precision has at least as much precision as
           double, but implementations may allow this default to be set by the
           user.
        */
        assert(result.ready());
        assert(result.size() == 2);

        if (result[1].matched)
        {
          assert(result[1].length() == 1);

          switch (*result[1].first)
          {
          case 'D': case 'd':
          case 'E': case 'e':
          default:
            return make(std::stod(literal));

          case 'F': case 'f':
            return make(std::stof(literal.substr().replace(result.position(1), 1, "e")));
          }
        }
        else
        {
          return make(std::stod(literal));
        }
      }
      else
      {
        throw std::invalid_argument("not a real number");
      }
    }
  }

  auto make_complex(std::string const& literal, int radix) -> object
  {
    try
    {
      return make_real(literal, radix);
    }
    catch (std::invalid_argument const&)
    {
      return make<complex>(literal, radix);
    }
  }

  auto make_number(std::string const& literal, int radix) -> object
  {
    try
    {
      return make_complex(literal, radix);
    }
    catch (std::invalid_argument const&)
    {
      throw std::invalid_argument("not a number");
    }
  }

namespace number
{
  auto equals(object const& x, object const& y) -> bool
  {
    return test<complex_numbers>(inexact_equals, x, y);
  }

  auto exact_integer_equals(object const& x, object const& y) -> bool
  {
    return test<exact_integers>(std::equal_to(), x, y);
  }

  auto not_equals(object const& x, object const& y) -> bool
  {
    return not equals(x, y);
  }

  auto less_than(object const& x, object const& y) -> bool
  {
    return test<real_numbers>(std::less(), x, y);
  }

  auto less_than_or_equals(object const& x, object const& y) -> bool
  {
    return not greater_than(x, y);
  }

  auto greater_than(object const& x, object const& y) -> bool
  {
    return test<real_numbers>(std::greater(), x, y);
  }

  auto greater_than_or_equals(object const& x, object const& y) -> bool
  {
    return not less_than(x, y);
  }

  auto exact(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return complex(exact(x.real()),
                       exact(x.imag()));
      }
      else if constexpr (std::is_floating_point_v<T>)
      {
        return ratio(std::forward<decltype(x)>(x));
      }
      else
      {
        return std::forward<decltype(x)>(x);
      }
    };

    return apply_to<complex_number>(f, x);
  }

  auto inexact(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return complex(inexact(x.real()),
                       inexact(x.imag()));
      }
      else
      {
        return static_cast<double>(std::forward<decltype(x)>(x));
      }
    };

    return apply_to<complex_number>(f, x);
  }

  auto is_complex(object const& x) -> bool
  {
    auto f = [](auto&&)
    {
      return true;
    };

    return test<complex_number>(f, x);
  }

  auto is_real(object const& x) -> bool
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return equals(x.imag(), e0);
      }
      else
      {
        return true;
      }
    };

    return test<complex_number>(f, x);
  }

  auto is_rational(object const& x) -> bool
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_floating_point_v<T>)
      {
        return not std::isnan(x) and
               not std::isinf(x);
      }
      else
      {
        return std::is_same_v<T, widen_integer> or
               std::is_same_v<T, large_integer> or
               std::is_same_v<T, ratio>;
      }
    };

    return test<complex_number>(f, x);
  }

  auto is_integer(object const& x) -> bool
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return equals(x.imag(), e0) and is_integer(x.real());
      }
      else if constexpr (std::is_floating_point_v<T>)
      {
        return x == std::trunc(x);
      }
      else if constexpr (std::is_same_v<T, ratio>)
      {
        return x.denominator() == 1_i64;
      }
      else
      {
        return std::is_same_v<T, widen_integer> or std::is_same_v<T, large_integer>;
      }
    };

    return test<complex_number>(f, x);
  }

  auto is_exact(object const& x) -> bool
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return is_exact(x.real()) and is_exact(x.imag());
      }
      else
      {
        return std::is_same_v<T, widen_integer> or std::is_same_v<T, large_integer> or  std::is_same_v<T, ratio>;
      }
    };

    return test<complex_number>(f, x);
  }

  auto is_inexact(object const& x) -> bool
  {
    return not is_exact(x);
  }

  auto is_exact_integer(object const& x) -> bool
  {
    auto f = []<typename T>(T const&)
    {
      static_assert(not std::is_same_v<T, small_integer>);
      return std::is_same_v<T, widen_integer> or std::is_same_v<T, large_integer>;
    };

    return test<complex_number>(f, x);
  }

  auto is_finite(object const& x) -> bool
  {
    return not is_infinite(x);
  }

  auto is_infinite(object const& x) -> bool
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return is_infinite(x.real()) or
               is_infinite(x.imag());
      }
      else if constexpr (std::is_floating_point_v<T>)
      {
        return std::isinf(std::forward<decltype(x)>(x));
      }
      else
      {
        return false;
      }
    };

    return test<complex_number>(f, x);
  }

  auto is_nan(object const& x) -> bool
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return is_nan(x.real()) or
               is_nan(x.imag());
      }
      else if constexpr (std::is_floating_point_v<T>)
      {
        return std::isnan(std::forward<decltype(x)>(x));
      }
      else
      {
        return false;
      }
    };

    return test<complex_number>(f, x);
  }

  auto is_zero(object const& x) -> bool
  {
    return equals(x, e0);
  }

  auto is_positive(object const& x) -> bool
  {
    return less_than(e0, x);
  }

  auto is_negative(object const& x) -> bool
  {
    return less_than(x, e0);
  }

  auto is_odd(object const& x) -> bool
  {
    return not is_even(x);
  }

  auto is_even(object const& x) -> bool
  {
    return is_zero(remainder(x, make<small_integer>(2)));
  }

  auto abs(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, large_integer>)
      {
        large_integer i {};
        mpz_abs(i.value, x.value);
        return i;
      }
      else if constexpr (std::is_floating_point_v<T>)
      {
        return std::fabs(std::forward<decltype(x)>(x));
      }
      else if constexpr (std::is_arithmetic_v<T>)
      {
        return std::abs(std::forward<decltype(x)>(x));
      }
      else if constexpr (std::is_same_v<T, complex>)
      {
        return sqrt(x.real() * x.real() + x.imag() * x.imag());
      }
      else
      {
        return x < 0_i64 ? 0_i64 - x : x;
      }
    };

    return apply_to<complex_number>(f, x);
  }

  auto quotient(object const& x, object const& y) -> object
  {
    return truncate(x / y);
  }

  auto remainder(object const& x, object const& y) -> object
  {
    return x % y;
  }

  auto modulo(object const& x, object const& y) -> object
  {
    return ((x % y) + y) % y;
  }

  auto gcd(object const& x, object const& y) -> object
  {
    return is_zero(y) ? abs(x) : gcd(y, remainder(x, y));
  }

  auto lcm(object const& x, object const& y) -> object
  {
    return abs(quotient(x * y, gcd(x, y)));
  }

  auto real(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return x.real();
      }
      else
      {
        return x;
      }
    };

    return apply_to<complex_number>(f, x);
  }

  auto imag(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return x.imag();
      }
      else
      {
        return e0;
      }
    };

    return apply_to<complex_number>(f, x);
  }

  auto magnitude(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return sqrt(x.real() * x.real() + x.imag() * x.imag());
      }
      else
      {
        return x;
      }
    };

    return apply_to<complex_number>(f, x);
  }

  auto angle(object const& x) -> object
  {
    return atan2(real(x), imag(x));
  }

  auto numerator(object const& x) -> object
  {
    if (x.is<ratio>())
    {
      return make(x.as<ratio>().numerator());
    }
    else if (is_exact(x))
    {
      return x;
    }
    else
    {
      return inexact(numerator(exact(x)));
    }
  }

  auto denominator(object const& x) -> object
  {
    if (x.is<ratio>())
    {
      return make(x.as<ratio>().denominator());
    }
    else if (is_exact(x))
    {
      return e1;
    }
    else if (is_integer(x))
    {
      return make(1.0);
    }
    else
    {
      return inexact(denominator(exact(x)));
    }
  }

  auto number_to_string(object const& x, int radix) -> object
  {
    auto f = [radix]<typename T>(T const& x)
    {
      if constexpr (std::is_floating_point_v<T>)
      {
        switch (radix)
        {
        case 2:
        case 8:
          return string("TODO");

        default:
        case 10:
          {
            char buffer[48] = {};
            std::to_chars(buffer, buffer + sizeof(buffer), x, std::chars_format::general);
            return string(buffer);
          }

        case 16:
          {
            auto buffer = std::ostringstream();
            buffer << std::hexfloat << x << std::defaultfloat;
            return string(buffer.str());
          }
        }
      }
      else if constexpr (std::is_same_v<T, widen_integer>)
      {
        char buffer[33] = {};
        std::to_chars(buffer, buffer + sizeof(buffer), x, radix);
        return string(buffer);
      }
      else if constexpr (std::is_same_v<T, large_integer>)
      {
        return string(std::unique_ptr<char, gmp_free>(mpz_get_str(nullptr, radix, x.value)).get());
      }
      else
      {
        return string("TODO");
      }
    };

    return apply_to<complex_number>(f, x);
  }
} // namespace number
} // namespace meevax::kernel
