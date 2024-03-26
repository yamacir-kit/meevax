/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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
#include <regex>
#include <string_view>

#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/utility/combination.hpp>

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

  template <typename T>
  auto canonicalize(T&& x) -> decltype(auto)
  {
    if constexpr (std::is_same_v<std::decay_t<T>, object> or
                  std::is_same_v<std::decay_t<T>, object::pointer>)
    {
      return std::forward<decltype(x)>(x);
    }
    else if constexpr (std::is_same_v<std::decay_t<T>, complex>)
    {
      return x.canonicalize();
    }
    else if constexpr (std::is_same_v<std::decay_t<T>, ratio>)
    {
      return x.denominator() == 1 ? make(x.numerator()) : make(std::forward<decltype(x)>(x));
    }
    else
    {
      return make(std::forward<decltype(x)>(x));
    }
  }

  template <auto I = 0, typename F>
  auto apply([[maybe_unused]] F f, object const& x) -> object
  {
    using Ts = std::tuple<exact_integer, ratio, float, double, complex>;

    if constexpr (I < std::tuple_size_v<Ts>)
    {
      using T = std::tuple_element_t<I, Ts>;

      return x.is<T>() ? canonicalize(f(x.as<T>())) : apply<I + 1>(f, x);
    }
    else
    {
      throw std::domain_error("not an number");
    }
  }

  template <auto I = 0, typename F>
  auto apply([[maybe_unused]] F f, object const& x, object const& y) -> object
  {
    using Ts = combination<exact_integer, ratio, float, double, complex>;

    if constexpr (I < std::tuple_size_v<Ts>)
    {
      using T = std::tuple_element_t<0, std::tuple_element_t<I, Ts>>;
      using U = std::tuple_element_t<1, std::tuple_element_t<I, Ts>>;

      return x.is<T>() and y.is<U>() ? canonicalize(f(x.as<T>(), y.as<U>())) : apply<I + 1>(f, x, y);
    }
    else
    {
      throw std::domain_error("not an number");
    }
  }

  template <auto I = 0, typename F>
  auto test([[maybe_unused]] F f, object const& x) -> bool
  {
    using Ts = std::tuple<exact_integer, ratio, float, double, complex>;

    if constexpr (I < std::tuple_size_v<Ts>)
    {
      using T = std::tuple_element_t<I, Ts>;

      return x.is<T>() ? f(x.as<T>()) : test<I + 1>(f, x);
    }
    else
    {
      return false;
    }
  }

  template <auto I = 0, typename F>
  auto test([[maybe_unused]] F f, object const& x, object const& y) -> bool
  {
    using Ts = combination<exact_integer, ratio, float, double, complex>;

    if constexpr (I < std::tuple_size_v<Ts>)
    {
      using T = std::tuple_element_t<0, std::tuple_element_t<I, Ts>>;
      using U = std::tuple_element_t<1, std::tuple_element_t<I, Ts>>;

      return x.is<T>() and y.is<U>() ? f(x.as<T>(), y.as<U>()) : test<I + 1>(f, x, y);
    }
    else
    {
      return false;
    }
  }

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

  auto make_integer(std::string const& literal, int radix) -> object
  {
    return make<exact_integer>(literal, radix);
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

        // SRFI-144
        { "fl-e",         M_E        },
        { "fl-log2-e",    M_LOG2E    },
        { "fl-log10-e",   M_LOG10E   },
        { "fl-log-2",     M_LN2      },
        { "fl-1/log-2",   M_LN2      },
        { "fl-log-10",    M_LN10     },
        { "fl-1/log-10",  M_LN10     },
        { "fl-pi",        M_PI       },
        { "fl-1/pi",      M_1_PI     },
        { "fl-pi/2",      M_PI_2     },
        { "fl-pi/4",      M_PI_4     },
        { "fl-2/pi",      M_2_PI     },
        { "fl-2/sqrt-pi", M_2_SQRTPI },
        { "fl-sqrt-2",    M_SQRT2    },
        { "fl-1/sqrt-2",  M_SQRT1_2  },
      };

      auto static const pattern = std::regex(R"(([+-]?(?:\d+\.?|\d*\.\d+))([DEFLSdefls][+-]?\d+)?)");

      if (auto iter = constants.find(literal); iter != constants.end())
      {
        return make(iter->second);
      }
      else if (std::regex_match(literal, pattern))
      {
        return make(lexical_cast<double>(literal));
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

  auto is_exact(object const& x) -> bool
  {
    auto f = [](auto const& x)
    {
      using T = std::decay_t<decltype(x)>;

      if constexpr (std::is_same_v<T, complex>)
      {
        return is_exact(x.real()) and is_exact(x.imag());
      }
      else
      {
        return std::is_same_v<T, ratio> or std::is_same_v<T, exact_integer>;
      }
    };

    return test(f, x);
  }

  auto is_inexact(object const& x) -> bool
  {
    return not is_exact(x);
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
    let static const e2 = make<exact_integer>(2);
    return is_zero(remainder(x, e2));
  }

  auto max(object const& xs) -> object
  {
    if (auto iter = std::max_element(xs.begin(), xs.end(), less_than); iter != xs.end())
    {
      return std::any_of(xs.begin(), xs.end(), is_inexact) ? inexact(*iter) : *iter;
    }
    else
    {
      return unspecified;
    }
  }

  auto min(object const& xs) -> object
  {
    if (auto iter = std::min_element(xs.begin(), xs.end(), less_than); iter != xs.end())
    {
      return std::any_of(xs.begin(), xs.end(), is_inexact) ? inexact(*iter) : *iter;
    }
    else
    {
      return unspecified;
    }
  }

  auto abs(object const& x) -> object
  {
    auto f = [](auto&& x)
    {
      using T = std::decay_t<decltype(x)>;

      if constexpr (std::is_same_v<T, exact_integer>)
      {
        exact_integer i {};
        mpz_abs(i.value, x.value);
        return i;
      }
      else if constexpr (std::is_arithmetic_v<T>)
      {
        return std::abs(std::forward<decltype(x)>(x));
      }
      else
      {
        static auto const zero = static_cast<exact_integer>(0);
        return x < zero ? zero - x : x;
      }
    };

    return apply(f, x);
  }

  auto quotient(object const& x, object const& y) -> object
  {
    return trunc(x / y);
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
            auto const [s, r] = x.square_root();

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
        throw std::invalid_argument("not a real number");
        return std::numeric_limits<double>::quiet_NaN();
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
