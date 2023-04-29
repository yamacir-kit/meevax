/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#include <memory> // std::unique_ptr

#include <meevax/kernel/number.hpp>
#include <meevax/kernel/string.hpp>

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
  auto operator % (exact_integer const&  , ratio const&  ) -> ratio { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(exact_integer const& a, ratio const& b) -> bool  { return 0 == mpq_cmp_z(b.value, a.value); }
  auto operator !=(exact_integer const& a, ratio const& b) -> bool  { return 0 != mpq_cmp_z(b.value, a.value); }
  auto operator < (exact_integer const& a, ratio const& b) -> bool  { return 0 <  mpq_cmp_z(b.value, a.value); }
  auto operator <=(exact_integer const& a, ratio const& b) -> bool  { return 0 <= mpq_cmp_z(b.value, a.value); }
  auto operator > (exact_integer const& a, ratio const& b) -> bool  { return 0 >  mpq_cmp_z(b.value, a.value); }
  auto operator >=(exact_integer const& a, ratio const& b) -> bool  { return 0 >= mpq_cmp_z(b.value, a.value); }

  auto operator + (exact_integer const& a, float b) -> float { return inexact_cast(a) +  b; }
  auto operator - (exact_integer const& a, float b) -> float { return inexact_cast(a) -  b; }
  auto operator * (exact_integer const& a, float b) -> float { return inexact_cast(a) *  b; }
  auto operator / (exact_integer const& a, float b) -> float { return inexact_cast(a) /  b; }
  auto operator % (exact_integer const& a, float b) -> float { return std::remainder(inexact_cast(a), b); }
  auto operator ==(exact_integer const& a, float b) -> bool  { return inexact_equals(inexact_cast(a), b); }
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

  auto operator + (exact_integer const& a, complex const& b) -> complex { return complex(make(a), e0) +  b; }
  auto operator - (exact_integer const& a, complex const& b) -> complex { return complex(make(a), e0) -  b; }
  auto operator * (exact_integer const& a, complex const& b) -> complex { return complex(make(a), e0) *  b; }
  auto operator / (exact_integer const& a, complex const& b) -> complex { return complex(make(a), e0) /  b; }
  auto operator % (exact_integer const&  , complex const&  ) -> complex { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(exact_integer const& a, complex const& b) -> bool    { return complex(make(a), e0) == b; }
  auto operator !=(exact_integer const& a, complex const& b) -> bool    { return complex(make(a), e0) != b; }
  auto operator < (exact_integer const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator <=(exact_integer const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator > (exact_integer const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator >=(exact_integer const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }

  auto operator + (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_add(q.value, a.value, ratio(b).value); return q; }
  auto operator - (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_sub(q.value, a.value, ratio(b).value); return q; }
  auto operator * (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_mul(q.value, a.value, ratio(b).value); return q; }
  auto operator / (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_div(q.value, a.value, ratio(b).value); return q; }
  auto operator % (ratio const&  , exact_integer const&  ) -> ratio { throw std::invalid_argument("unsupported operation"); }
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
  auto operator % (ratio const&  , ratio const&  ) -> ratio { throw std::invalid_argument("unsupported operation"); }
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
  auto operator ==(ratio const& a, float b) -> bool  { return inexact_equals(inexact_cast(a), b); }
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
  auto operator ==(ratio const& a, double b) -> bool   { return inexact_equals(inexact_cast(a), b); }
  auto operator !=(ratio const& a, double b) -> bool   { return not (a == b); }
  auto operator < (ratio const& a, double b) -> bool   { return inexact_cast(a) <  b; }
  auto operator <=(ratio const& a, double b) -> bool   { return inexact_cast(a) <= b; }
  auto operator > (ratio const& a, double b) -> bool   { return inexact_cast(a) >  b; }
  auto operator >=(ratio const& a, double b) -> bool   { return inexact_cast(a) >= b; }

  auto operator + (ratio const& a, complex const& b) -> complex { return complex(make(a), e0) +  b; }
  auto operator - (ratio const& a, complex const& b) -> complex { return complex(make(a), e0) -  b; }
  auto operator * (ratio const& a, complex const& b) -> complex { return complex(make(a), e0) *  b; }
  auto operator / (ratio const& a, complex const& b) -> complex { return complex(make(a), e0) /  b; }
  auto operator % (ratio const&  , complex const&  ) -> complex { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(ratio const& a, complex const& b) -> bool    { return complex(make(a), e0) == b; }
  auto operator !=(ratio const& a, complex const& b) -> bool    { return complex(make(a), e0) != b; }
  auto operator < (ratio const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator <=(ratio const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator > (ratio const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator >=(ratio const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }

  auto operator + (float a, exact_integer const& b) -> float { return a +  inexact_cast(b); }
  auto operator - (float a, exact_integer const& b) -> float { return a -  inexact_cast(b); }
  auto operator * (float a, exact_integer const& b) -> float { return a *  inexact_cast(b); }
  auto operator / (float a, exact_integer const& b) -> float { return a /  inexact_cast(b); }
  auto operator % (float a, exact_integer const& b) -> float { return std::remainder(a, inexact_cast(b)); }
  auto operator ==(float a, exact_integer const& b) -> bool  { return inexact_equals(a, inexact_cast(b)); }
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
  auto operator ==(float a, ratio const& b) -> bool  { return inexact_equals(a, inexact_cast(b)); }
  auto operator !=(float a, ratio const& b) -> bool  { return not (a == b); }
  auto operator < (float a, ratio const& b) -> bool  { return a <  inexact_cast(b); }
  auto operator <=(float a, ratio const& b) -> bool  { return a <= inexact_cast(b); }
  auto operator > (float a, ratio const& b) -> bool  { return a >  inexact_cast(b); }
  auto operator >=(float a, ratio const& b) -> bool  { return a >= inexact_cast(b); }

  auto operator + (float a, complex const& b) -> complex { return complex(make(a), e0) +  b; }
  auto operator - (float a, complex const& b) -> complex { return complex(make(a), e0) -  b; }
  auto operator * (float a, complex const& b) -> complex { return complex(make(a), e0) *  b; }
  auto operator / (float a, complex const& b) -> complex { return complex(make(a), e0) /  b; }
  auto operator % (float  , complex const&  ) -> complex { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(float a, complex const& b) -> bool    { return complex(make(a), e0) == b; }
  auto operator !=(float a, complex const& b) -> bool    { return complex(make(a), e0) != b; }
  auto operator < (float  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator <=(float  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator > (float  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator >=(float  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }

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
  auto operator ==(double a, ratio const& b) -> bool   { return inexact_equals(a, inexact_cast(b)); }
  auto operator !=(double a, ratio const& b) -> bool   { return not (a == b); }
  auto operator < (double a, ratio const& b) -> bool   { return a <  inexact_cast(b); }
  auto operator <=(double a, ratio const& b) -> bool   { return a <= inexact_cast(b); }
  auto operator > (double a, ratio const& b) -> bool   { return a >  inexact_cast(b); }
  auto operator >=(double a, ratio const& b) -> bool   { return a >= inexact_cast(b); }

  auto operator + (double a, complex const& b) -> complex { return complex(make(a), e0) +  b; }
  auto operator - (double a, complex const& b) -> complex { return complex(make(a), e0) -  b; }
  auto operator * (double a, complex const& b) -> complex { return complex(make(a), e0) *  b; }
  auto operator / (double a, complex const& b) -> complex { return complex(make(a), e0) /  b; }
  auto operator % (double  , complex const&  ) -> complex { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(double a, complex const& b) -> bool    { return complex(make(a), e0) == b; }
  auto operator !=(double a, complex const& b) -> bool    { return complex(make(a), e0) != b; }
  auto operator < (double  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator <=(double  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator > (double  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator >=(double  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }

  auto operator + (complex const& a, complex const& b) -> complex { return complex(a.real() + b.real(), a.imag() + b.imag()); }
  auto operator - (complex const& a, complex const& b) -> complex { return complex(a.real() - b.real(), a.imag() - b.imag()); }
  auto operator * (complex const& a, complex const& b) -> complex { return complex(a.real() * b.real() - a.imag() * b.imag(), a.imag() * b.real() + a.real() * b.imag()); }
  auto operator / (complex const& a, complex const& b) -> complex { auto x = a.real() * b.real() + a.imag() * b.imag(); auto y = a.imag() * b.real() - a.real() * b.imag(); auto d = b.real() * b.real() + b.imag() * b.imag(); return complex(x / d, y / d); }
  auto operator % (complex const&  , complex const&  ) -> complex { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(complex const& a, complex const& b) -> bool    { return equals(a.real(), b.real()) and equals(a.imag(), b.imag()); }
  auto operator !=(complex const& a, complex const& b) -> bool    { return not (a == b); }
  auto operator < (complex const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator <=(complex const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator > (complex const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator >=(complex const&  , complex const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }

  auto operator + (complex const& a, float b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, float b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, float b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, float b) -> complex { return a /  complex(make(b), e0); }
  auto operator % (complex const&  , float  ) -> complex { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(complex const& a, float b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, float b) -> bool    { return a != complex(make(b), e0); }
  auto operator < (complex const&  , float  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator <=(complex const&  , float  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator > (complex const&  , float  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator >=(complex const&  , float  ) -> bool    { throw std::invalid_argument("unsupported operation"); }

  auto operator + (complex const& a, double b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, double b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, double b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, double b) -> complex { return a /  complex(make(b), e0); }
  auto operator % (complex const&  , double  ) -> complex { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(complex const& a, double b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, double b) -> bool    { return a != complex(make(b), e0); }
  auto operator < (complex const&  , double  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator <=(complex const&  , double  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator > (complex const&  , double  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator >=(complex const&  , double  ) -> bool    { throw std::invalid_argument("unsupported operation"); }

  auto operator + (complex const& a, ratio const& b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, ratio const& b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, ratio const& b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, ratio const& b) -> complex { return a /  complex(make(b), e0); }
  auto operator % (complex const&  , ratio const&  ) -> complex { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(complex const& a, ratio const& b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, ratio const& b) -> bool    { return a != complex(make(b), e0); }
  auto operator < (complex const&  , ratio const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator <=(complex const&  , ratio const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator > (complex const&  , ratio const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator >=(complex const&  , ratio const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }

  auto operator + (complex const& a, exact_integer const& b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, exact_integer const& b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, exact_integer const& b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, exact_integer const& b) -> complex { return a /  complex(make(b), e0); }
  auto operator % (complex const&  , exact_integer const&  ) -> complex { throw std::invalid_argument("unsupported operation"); }
  auto operator ==(complex const& a, exact_integer const& b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, exact_integer const& b) -> bool    { return a != complex(make(b), e0); }
  auto operator < (complex const&  , exact_integer const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator <=(complex const&  , exact_integer const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator > (complex const&  , exact_integer const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }
  auto operator >=(complex const&  , exact_integer const&  ) -> bool    { throw std::invalid_argument("unsupported operation"); }

  auto operator +(object const& x, object const& y) -> object
  {
    return apply(std::plus(), x, y);
  }

  auto operator -(object const& x, object const& y) -> object
  {
    return apply(std::minus(), x, y);
  }

  auto operator *(object const& x, object const& y) -> object
  {
    return apply(std::multiplies(), x, y);
  }

  auto operator /(object const& x, object const& y) -> object
  {
    return apply(std::divides(), x, y);
  }

  auto operator % (object const& x, object const& y) -> object
  {
    auto f = [](auto&& x, auto&& y)
    {
      using T = std::decay_t<decltype(x)>;
      using U = std::decay_t<decltype(y)>;

      if constexpr (std::is_floating_point_v<T> and
                    std::is_floating_point_v<U>)
      {
        return std::fmod(x, y);
      }
      else
      {
        return x % y;
      }
    };

    return apply(f, x, y);
  }

inline namespace number
{
  auto equals(object const& x, object const& y) -> bool
  {
    auto f = [](auto&&... xs)
    {
      return inexact_equals(std::forward<decltype(xs)>(xs)...);
    };

    return test(f, x, y);
  }

  auto not_equals(object const& x, object const& y) -> bool
  {
    return not equals(x, y);
  }

  auto less_than(object const& x, object const& y) -> bool
  {
    return test(std::less(), x, y);
  }

  auto less_than_or_equals(object const& x, object const& y) -> bool
  {
    return not greater_than(x, y);
  }

  auto greater_than(object const& x, object const& y) -> bool
  {
    return test(std::greater(), x, y);
  }

  auto greater_than_or_equals(object const& x, object const& y) -> bool
  {
    return not less_than(x, y);
  }

  auto exact(object const& x) -> object
  {
    auto f = [](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

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

    return apply(f, x);
  }

  auto inexact(object const& x) -> object
  {
    auto f = [](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

      if constexpr (std::is_same_v<T, complex>)
      {
        return complex(inexact(x.real()),
                       inexact(x.imag()));
      }
      else
      {
        return inexact_cast(std::forward<decltype(x)>(x));
      }
    };

    return apply(f, x);
  }

  auto is_complex(object const& x) -> bool
  {
    auto f = [](auto&&)
    {
      return true;
    };

    return test(f, x);
  }

  auto is_real(object const& x) -> bool
  {
    auto f = [](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

      if constexpr (std::is_same_v<T, complex>)
      {
        return equals(x.imag(), e0);
      }
      else
      {
        return true;
      }
    };

    return test(f, x);
  }

  auto is_rational(object const& x) -> bool
  {
    auto f = [](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

      if constexpr (std::is_floating_point_v<T>)
      {
        return not std::isnan(x) and
               not std::isinf(x);
      }
      else
      {
        return std::is_same_v<T, exact_integer> or
               std::is_same_v<T, ratio>;
      }
    };

    return test(f, x);
  }

  auto is_integer(object const& x) -> bool
  {
    auto f = [](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

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
        return x.denominator() == 1;
      }
      else
      {
        return std::is_same_v<T, exact_integer>;
      }
    };

    return test(f, x);
  }

  auto is_finite(object const& x) -> bool
  {
    return not is_infinite(x);
  }

  auto is_infinite(object const& x) -> bool
  {
    auto f = [](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

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

    return test(f, x);
  }

  auto is_nan(object const& x) -> bool
  {
    auto f = [](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

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

    return test(f, x);
  }

  auto sqrt(object const& x) -> object
  {
    auto f = [](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

      if constexpr (std::is_same_v<T, complex>)
      {
        auto const z = std::sqrt(inexact_cast(std::forward<decltype(x)>(x)));

        return complex(make(z.real()),
                       make(z.imag()));
      }
      else
      {
        auto sqrt = [](auto&& x)
        {
          if constexpr (std::is_same_v<T, exact_integer>)
          {
            auto const [s, r] = exact_integer_sqrt(x);

            return r == 0 ? make(s) : make(std::sqrt(inexact_cast(x)));
          }
          else
          {
            return make(std::sqrt(inexact_cast(std::forward<decltype(x)>(x))));
          }
        };

        return x < exact_integer(0) ? make<complex>(e0, sqrt(exact_integer(0) - x))
                                    : sqrt(x);
      }
    };

    return apply(f, x);
  }

  auto pow(object const& x, object const& y) -> object
  {
    auto f = [](auto&& x, auto&& y)
    {
      using T = std::decay_t<decltype(x)>;
      using U = std::decay_t<decltype(y)>;

      if constexpr (std::is_same_v<T, complex> or
                    std::is_same_v<U, complex>)
      {
        auto const z = std::pow(inexact_cast(std::forward<decltype(x)>(x)),
                                inexact_cast(std::forward<decltype(y)>(y)));

        return complex(make(z.real()),
                       make(z.imag()));
      }
      else if constexpr (std::is_same_v<T, exact_integer> and
                         std::is_same_v<U, exact_integer>)
      {
        exact_integer result {};
        mpz_pow_ui(result.value, x.value, static_cast<unsigned long>(y));
        return result;
      }
      else
      {
        return std::pow(inexact_cast(std::forward<decltype(x)>(x)),
                        inexact_cast(std::forward<decltype(y)>(y)));
      }
    };

    return apply(f, x, y);
  }

  #define DEFINE(ROUND)                                                        \
  auto ROUND(object const& x) -> object                                        \
  {                                                                            \
    auto f = [](auto&& x)                                                      \
    {                                                                          \
      using T = std::decay_t<decltype(x)>;                                     \
                                                                               \
      if constexpr (std::is_floating_point_v<T>)                               \
      {                                                                        \
        return std::ROUND(inexact_cast(std::forward<decltype(x)>(x)));         \
      }                                                                        \
      else if constexpr (std::is_same_v<T, ratio>)                             \
      {                                                                        \
        return exact_integer(std::ROUND(inexact_cast(std::forward<decltype(x)>(x)))); \
      }                                                                        \
      else if constexpr (std::is_same_v<T, exact_integer>)                     \
      {                                                                        \
        return std::forward<decltype(x)>(x);                                   \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return complex(ROUND(x.real()),                                        \
                       ROUND(x.imag()));                                       \
      }                                                                        \
    };                                                                         \
                                                                               \
    return apply(f, x);                                                        \
  }                                                                            \
  static_assert(true)

  DEFINE(floor);
  DEFINE(ceil);
  DEFINE(trunc);
  DEFINE(round);

  #undef DEFINE

  #define DEFINE(CMATH)                                                        \
  auto CMATH(object const& x) -> object                                        \
  {                                                                            \
    auto f = [](auto&& x)                                                      \
    {                                                                          \
      using T = std::decay_t<decltype(x)>;                                     \
                                                                               \
      if constexpr (std::is_same_v<T, complex>)                                \
      {                                                                        \
        auto const z = std::CMATH(inexact_cast(std::forward<decltype(x)>(x))); \
                                                                               \
        return complex(make(z.real()),                                         \
                       make(z.imag()));                                        \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return std::CMATH(inexact_cast(std::forward<decltype(x)>(x)));         \
      }                                                                        \
    };                                                                         \
                                                                               \
    return apply(f, x);                                                        \
  }                                                                            \
  static_assert(true)

  DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh);
  DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh);
  DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh);

  DEFINE(exp);
  DEFINE(log);

  #undef DEFINE

  auto atan(object const& x, object const& y) -> object
  {
    auto f = [](auto&& x, auto&& y)
    {
      using T = std::decay_t<decltype(x)>;
      using U = std::decay_t<decltype(y)>;

      if constexpr (std::is_same_v<T, complex> or
                    std::is_same_v<U, complex>)
      {
        throw std::invalid_argument("unsupported operation");
        return e0; // dummy return value.
      }
      else
      {
        return std::atan2(inexact_cast(std::forward<decltype(x)>(x)),
                          inexact_cast(std::forward<decltype(y)>(y)));
      }
    };

    return apply(f, x, y);
  }

  auto number_to_string(object const& x, int radix) -> object
  {
    auto f = [radix](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

      if constexpr (std::is_floating_point_v<T>)
      {
        return string("TODO");
      }
      else if constexpr (std::is_same_v<T, exact_integer>)
      {
        return string(std::unique_ptr<char, gmp_free>(mpz_get_str(nullptr, radix, x.value)).get());
      }
      else
      {
        return string("TODO");
      }
    };

    return apply(f, x);
  }
} // namespace number
} // namespace kernel
} // namespace meevax
