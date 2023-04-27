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
  auto operator ==(double a, ratio const& b) -> bool   { return std::invoke(equal_to(), a, inexact_cast(b)); }
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
  auto operator ==(complex const& a, complex const& b) -> bool    { return arithmetic::apply<equal_to>(a.real(), b.real()).as<bool>() and arithmetic::apply<equal_to>(a.imag(), b.imag()).as<bool>(); }
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

  struct modulus
  {
    template <typename T, typename U>
    auto operator ()(T&& x, U&& y) const
    {
      if constexpr (std::is_floating_point_v<std::decay_t<T>> and
                    std::is_floating_point_v<std::decay_t<U>>)
      {
        return std::fmod(x, y);
      }
      else
      {
        return x % y;
      }
    }
  };

  auto operator + (object const& x, object const& y) -> object { return arithmetic::apply<std::plus      <void>>(x, y); }
  auto operator - (object const& x, object const& y) -> object { return arithmetic::apply<std::minus     <void>>(x, y); }
  auto operator * (object const& x, object const& y) -> object { return arithmetic::apply<std::multiplies<void>>(x, y); }
  auto operator / (object const& x, object const& y) -> object { return arithmetic::apply<std::divides   <void>>(x, y); }
  auto operator % (object const& x, object const& y) -> object { return arithmetic::apply<     modulus         >(x, y); }

  struct exact_t
  {
    template <typename T>
    auto operator ()(T const& x) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return complex(exact(x.real()),
                       exact(x.imag()));
      }
      else if constexpr (std::is_floating_point_v<T>)
      {
        return ratio(x);
      }
      else
      {
        return x;
      }
    }
  };

  auto exact(object const& x) -> object
  {
    return arithmetic::apply<exact_t>(x);
  }

  struct inexact_t
  {
    template <typename T>
    auto operator ()(T const& x) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return complex(inexact(x.real()),
                       inexact(x.imag()));
      }
      else
      {
        return inexact_cast(x);
      }
    }
  };

  auto inexact(object const& x) -> object
  {
    return arithmetic::apply<inexact_t>(x);
  }

  struct is_complex_t
  {
    template <typename T>
    constexpr auto operator ()(T const&) const
    {
      return true;
    }
  };

  auto is_complex(object const& x) -> bool
  {
    try
    {
      return arithmetic::apply<is_complex_t>(x).as<bool>();
    }
    catch (std::out_of_range const&)
    {
      return false;
    }
  }

  struct is_real_t
  {
    template <typename T>
    constexpr auto operator ()(T const& x) const
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return arithmetic::apply<equal_to>(x.imag(), e0).template as<bool>();
      }
      else
      {
        return true;
      }
    }
  };

  auto is_real(object const& x) -> bool
  {
    try
    {
      return arithmetic::apply<is_real_t>(x).as<bool>();
    }
    catch (std::out_of_range const&)
    {
      return false;
    }
  }

  struct is_rational_t
  {
    template <typename T>
    constexpr auto operator ()(T const& x) const
    {
      if constexpr (std::is_floating_point_v<T>)
      {
        return not std::isnan(x) and not std::isinf(x);
      }
      else
      {
        return std::is_same_v<T, exact_integer> or
               std::is_same_v<T, ratio>;
      }
    }
  };

  auto is_rational(object const& x) -> bool
  {
    try
    {
      return arithmetic::apply<is_rational_t>(x).as<bool>();
    }
    catch (std::out_of_range const&)
    {
      return false;
    }
  }

  struct is_integer_t
  {
    template <typename T>
    constexpr auto operator ()(T const& x) const
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return arithmetic::apply<equal_to>(x.imag(), e0).template as<bool>() and is_integer(x.real());
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
    }
  };

  auto is_integer(object const& x) -> bool
  {
    try
    {
      return arithmetic::apply<is_integer_t>(x).as<bool>();
    }
    catch (std::out_of_range const&)
    {
      return false;
    }
  }

  auto is_finite(object const& a) -> bool
  {
    return not is_infinite(a);
  }

  struct is_infinite_t
  {
    template <typename T>
    constexpr auto operator ()(T const& x) const
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return is_infinite(x.real()) or
               is_infinite(x.imag());
      }
      else if constexpr (std::is_floating_point_v<T>)
      {
        return std::isinf(x);
      }
      else
      {
        return false;
      }
    }
  };

  auto is_infinite(object const& x) -> bool
  {
    try
    {
      return arithmetic::apply<is_infinite_t>(x).as<bool>();
    }
    catch (std::out_of_range const&)
    {
      return false;
    }
  }

  struct is_nan_t
  {
    template <typename T>
    constexpr auto operator ()(T const& x) const
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        return is_nan(x.real()) or is_nan(x.imag());
      }
      else if constexpr (std::is_floating_point_v<T>)
      {
        return std::isnan(x);
      }
      else
      {
        return false;
      }
    }
  };

  auto is_nan(object const& a) -> bool
  {
    try
    {
      return arithmetic::apply<is_nan_t>(a).as<bool>();
    }
    catch (std::out_of_range const&)
    {
      return false;
    }
  }

  struct sqrt_t
  {
    template <typename T>
    constexpr auto operator ()(T&& x) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)
      {
        auto const z = std::sqrt(inexact_cast(std::forward<decltype(x)>(x)));
        return complex(make(z.real()),
                       make(z.imag()));
      }
      else
      {
        auto sqrt = [](auto&& x)
        {
          if constexpr (std::is_same_v<std::decay_t<decltype(x)>, exact_integer>)
          {
            auto const [s, r] = exact_integer_sqrt(x);
            return r == 0 ? make(s) : make(std::sqrt(inexact_cast(x)));
          }
          else
          {
            return make(std::sqrt(inexact_cast(x)));
          }
        };

        return x < exact_integer(0) ? make<complex>(e0, sqrt(exact_integer(0) - x))
                                    : sqrt(x);
      }
    }
  };

  auto sqrt(object const& x) -> object
  {
    return arithmetic::apply<sqrt_t>(x);
  }

  struct pow_t
  {
    template <typename T, typename U>
    auto operator ()(T&& x, U&& y) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex> or
                    std::is_same_v<std::decay_t<decltype(y)>, complex>)
      {
        auto const z = std::pow(inexact_cast(std::forward<decltype(x)>(x)),
                                inexact_cast(std::forward<decltype(y)>(y)));
        return complex(make(z.real()), make(z.imag()));
      }
      else if constexpr (std::is_same_v<std::decay_t<decltype(x)>, exact_integer> and
                         std::is_same_v<std::decay_t<decltype(y)>, exact_integer>)
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
    }
  };

  auto pow(object const& a, object const& b) -> object
  {
    static const auto apply = arithmetic::apply_t<pow_t, 2>();
    return apply(a, b);
  }

  #define DEFINE(ROUND)                                                        \
  struct ROUND##_t                                                             \
  {                                                                            \
    template <typename T>                                                      \
    constexpr auto operator ()(T&& x) const                                    \
    {                                                                          \
      if constexpr (std::is_floating_point_v<std::decay_t<T>>)                 \
      {                                                                        \
        return std::ROUND(inexact_cast(std::forward<decltype(x)>(x)));         \
      }                                                                        \
      else if constexpr (std::is_same_v<std::decay_t<T>, ratio>)               \
      {                                                                        \
        return exact_integer(std::ROUND(inexact_cast(std::forward<decltype(x)>(x)))); \
      }                                                                        \
      else if constexpr (std::is_same_v<std::decay_t<T>, exact_integer>)       \
      {                                                                        \
        return std::forward<decltype(x)>(x);                                   \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return complex(arithmetic::apply<ROUND##_t>(x.real()),                 \
                       arithmetic::apply<ROUND##_t>(x.imag()));                \
      }                                                                        \
    }                                                                          \
  }

  DEFINE(floor);
  DEFINE(ceil);
  DEFINE(trunc);
  DEFINE(round);

  #undef DEFINE

  auto floor(object const& x) -> object
  {
    return arithmetic::apply<floor_t>(x);
  }

  auto ceil(object const& x) -> object
  {
    return arithmetic::apply<ceil_t>(x);
  }

  auto trunc(object const& x) -> object
  {
    return arithmetic::apply<trunc_t>(x);
  }

  auto round(object const& x) -> object
  {
    return arithmetic::apply<round_t>(x);
  }

  #define DEFINE(CMATH)                                                        \
  struct CMATH##_t                                                             \
  {                                                                            \
    template <typename T>                                                      \
    auto operator ()(T&& x) const                                              \
    {                                                                          \
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)        \
      {                                                                        \
        auto const z = std::CMATH(inexact_cast(std::forward<decltype(x)>(x))); \
        return complex(make(z.real()),                                         \
                       make(z.imag()));                                        \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return std::CMATH(inexact_cast(std::forward<decltype(x)>(x)));         \
      }                                                                        \
    }                                                                          \
  }

  DEFINE(sin); DEFINE(asin); DEFINE(sinh); DEFINE(asinh);
  DEFINE(cos); DEFINE(acos); DEFINE(cosh); DEFINE(acosh);
  DEFINE(tan); DEFINE(atan); DEFINE(tanh); DEFINE(atanh);

  DEFINE(exp);
  DEFINE(log);

  #undef DEFINE

  struct atan2_t
  {
    template <typename T, typename U>
    auto operator ()(T&& x, U&& y) const -> decltype(auto)
    {
      if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex> or
                    std::is_same_v<std::decay_t<decltype(y)>, complex>)
      {
        throw std::invalid_argument("unsupported operation");
        return e0; // dummy return value.
      }
      else
      {
        return std::atan2(inexact_cast(std::forward<decltype(x)>(x)),
                          inexact_cast(std::forward<decltype(y)>(y)));
      }
    }
  };

  auto sin(object const& x) -> object
  {
    return arithmetic::apply<sin_t>(x);
  }

  auto cos(object const& x) -> object
  {
    return arithmetic::apply<cos_t>(x);
  }

  auto tan(object const& x) -> object
  {
    return arithmetic::apply<tan_t>(x);
  }

  auto asin(object const& x) -> object
  {
    return arithmetic::apply<asin_t>(x);
  }

  auto acos(object const& x) -> object
  {
    return arithmetic::apply<acos_t>(x);
  }

  auto atan(object const& x) -> object
  {
    return arithmetic::apply<atan_t>(x);
  }

  auto atan(object const& a, object const& b) -> object
  {
    static const auto apply = arithmetic::apply_t<atan2_t, 2>();
    return apply(a, b);
  }

  auto sinh(object const& x) -> object
  {
    return arithmetic::apply<sinh_t>(x);
  }

  auto cosh(object const& x) -> object
  {
    return arithmetic::apply<cosh_t>(x);
  }

  auto tanh(object const& x) -> object
  {
    return arithmetic::apply<tanh_t>(x);
  }

  auto asinh(object const& x) -> object
  {
    return arithmetic::apply<asinh_t>(x);
  }

  auto acosh(object const& x) -> object
  {
    return arithmetic::apply<acosh_t>(x);
  }

  auto atanh(object const& x) -> object
  {
    return arithmetic::apply<atanh_t>(x);
  }

  auto exp(object const& x) -> object
  {
    return arithmetic::apply<exp_t>(x);
  }

  auto log(object const& x) -> object
  {
    return arithmetic::apply<log_t>(x);
  }
} // namespace kernel
} // namespace meevax
