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

#include <meevax/kernel/error.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/kernel/string.hpp>
#include <meevax/utility/combination.hpp>

namespace meevax::inline kernel
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
  auto operator % (exact_integer const&  , ratio const&  ) -> ratio { throw std::invalid_argument("unimplemented operation"); }
  auto operator ==(exact_integer const& a, ratio const& b) -> bool  { return 0 == mpq_cmp_z(b.value, a.value); }
  auto operator !=(exact_integer const& a, ratio const& b) -> bool  { return 0 != mpq_cmp_z(b.value, a.value); }
  auto operator < (exact_integer const& a, ratio const& b) -> bool  { return 0 <  mpq_cmp_z(b.value, a.value); }
  auto operator <=(exact_integer const& a, ratio const& b) -> bool  { return 0 <= mpq_cmp_z(b.value, a.value); }
  auto operator > (exact_integer const& a, ratio const& b) -> bool  { return 0 >  mpq_cmp_z(b.value, a.value); }
  auto operator >=(exact_integer const& a, ratio const& b) -> bool  { return 0 >= mpq_cmp_z(b.value, a.value); }

  auto operator + (exact_integer const& a, float b) -> float { return static_cast<float>(a) +  b; }
  auto operator - (exact_integer const& a, float b) -> float { return static_cast<float>(a) -  b; }
  auto operator * (exact_integer const& a, float b) -> float { return static_cast<float>(a) *  b; }
  auto operator / (exact_integer const& a, float b) -> float { return static_cast<float>(a) /  b; }
  auto operator % (exact_integer const& a, float b) -> float { return std::remainder(static_cast<float>(a), b); }
  auto operator ==(exact_integer const& a, float b) -> bool  { return inexact_equals(static_cast<float>(a), b); }
  auto operator !=(exact_integer const& a, float b) -> bool  { return not (a == b); }
  auto operator < (exact_integer const& a, float b) -> bool  { return static_cast<float>(a) <  b; }
  auto operator <=(exact_integer const& a, float b) -> bool  { return static_cast<float>(a) <= b; }
  auto operator > (exact_integer const& a, float b) -> bool  { return static_cast<float>(a) >  b; }
  auto operator >=(exact_integer const& a, float b) -> bool  { return static_cast<float>(a) >= b; }

  auto operator + (exact_integer const& a, double b) -> double { return static_cast<double>(a) +  b; }
  auto operator - (exact_integer const& a, double b) -> double { return static_cast<double>(a) -  b; }
  auto operator * (exact_integer const& a, double b) -> double { return static_cast<double>(a) *  b; }
  auto operator / (exact_integer const& a, double b) -> double { return static_cast<double>(a) /  b; }
  auto operator % (exact_integer const& a, double b) -> double { return std::remainder(static_cast<double>(a), b); }
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
  auto operator ==(exact_integer const& a, complex const& b) -> bool    { return complex(make(a), e0) == b; }
  auto operator !=(exact_integer const& a, complex const& b) -> bool    { return complex(make(a), e0) != b; }

  auto operator + (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_add(q.value, a.value, ratio(b).value); return q; }
  auto operator - (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_sub(q.value, a.value, ratio(b).value); return q; }
  auto operator * (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_mul(q.value, a.value, ratio(b).value); return q; }
  auto operator / (ratio const& a, exact_integer const& b) -> ratio { ratio q; mpq_div(q.value, a.value, ratio(b).value); return q; }
  auto operator % (ratio const&  , exact_integer const&  ) -> ratio { throw std::invalid_argument("unimplemented operation"); }
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

  auto operator + (float a, exact_integer const& b) -> float { return a +  static_cast<float>(b); }
  auto operator - (float a, exact_integer const& b) -> float { return a -  static_cast<float>(b); }
  auto operator * (float a, exact_integer const& b) -> float { return a *  static_cast<float>(b); }
  auto operator / (float a, exact_integer const& b) -> float { return a /  static_cast<float>(b); }
  auto operator % (float a, exact_integer const& b) -> float { return std::remainder(a, static_cast<float>(b)); }
  auto operator ==(float a, exact_integer const& b) -> bool  { return inexact_equals(a, static_cast<float>(b)); }
  auto operator !=(float a, exact_integer const& b) -> bool  { return not (a == b); }
  auto operator < (float a, exact_integer const& b) -> bool  { return a <  static_cast<float>(b); }
  auto operator <=(float a, exact_integer const& b) -> bool  { return a <= static_cast<float>(b); }
  auto operator > (float a, exact_integer const& b) -> bool  { return a >  static_cast<float>(b); }
  auto operator >=(float a, exact_integer const& b) -> bool  { return a >= static_cast<float>(b); }

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

  auto operator + (double a, exact_integer const& b) -> double { return a + static_cast<double>(b); }
  auto operator - (double a, exact_integer const& b) -> double { return a - static_cast<double>(b); }
  auto operator * (double a, exact_integer const& b) -> double { return a * static_cast<double>(b); }
  auto operator / (double a, exact_integer const& b) -> double { return a / static_cast<double>(b); }
  auto operator % (double a, exact_integer const& b) -> double { return std::remainder(a, static_cast<double>(b)); }
  auto operator ==(double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) == 0; }
  auto operator !=(double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) != 0; }
  auto operator < (double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) >  0; }
  auto operator <=(double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) >= 0; }
  auto operator > (double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) <  0; }
  auto operator >=(double a, exact_integer const& b) -> bool   { return mpz_cmp_d(b.value, a) <= 0; }

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

  auto operator + (complex const& a, complex const& b) -> complex { return complex(a.real() + b.real(), a.imag() + b.imag()); }
  auto operator - (complex const& a, complex const& b) -> complex { return complex(a.real() - b.real(), a.imag() - b.imag()); }
  auto operator * (complex const& a, complex const& b) -> complex { return complex(a.real() * b.real() - a.imag() * b.imag(), a.imag() * b.real() + a.real() * b.imag()); }
  auto operator / (complex const& a, complex const& b) -> complex { auto x = a.real() * b.real() + a.imag() * b.imag(); auto y = a.imag() * b.real() - a.real() * b.imag(); auto d = b.real() * b.real() + b.imag() * b.imag(); return complex(x / d, y / d); }
  auto operator ==(complex const& a, complex const& b) -> bool    { return equals(a.real(), b.real()) and equals(a.imag(), b.imag()); }
  auto operator !=(complex const& a, complex const& b) -> bool    { return not (a == b); }

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

  auto operator + (complex const& a, ratio const& b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, ratio const& b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, ratio const& b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, ratio const& b) -> complex { return a /  complex(make(b), e0); }
  auto operator ==(complex const& a, ratio const& b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, ratio const& b) -> bool    { return a != complex(make(b), e0); }

  auto operator + (complex const& a, exact_integer const& b) -> complex { return a +  complex(make(b), e0); }
  auto operator - (complex const& a, exact_integer const& b) -> complex { return a -  complex(make(b), e0); }
  auto operator * (complex const& a, exact_integer const& b) -> complex { return a *  complex(make(b), e0); }
  auto operator / (complex const& a, exact_integer const& b) -> complex { return a /  complex(make(b), e0); }
  auto operator ==(complex const& a, exact_integer const& b) -> bool    { return a == complex(make(b), e0); }
  auto operator !=(complex const& a, exact_integer const& b) -> bool    { return a != complex(make(b), e0); }

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

  using complex_number = std::tuple<exact_integer, ratio, float, double, complex>;

  using complex_numbers = combination<exact_integer, ratio, float, double, complex>;

  using real_number = std::tuple<exact_integer, ratio, float, double>;

  using real_numbers = combination<exact_integer, ratio, float, double>;

  template <typename Tuple, auto I = 0, typename F>
  auto apply_to([[maybe_unused]] F f, object const& x) -> object
  {
    if constexpr (I < std::tuple_size_v<Tuple>)
    {
      using type_i = std::tuple_element_t<I, Tuple>;

      return x.is<type_i>() ? canonicalize(f(x.as<type_i>())) : apply_to<Tuple, I + 1>(f, x);
    }
    else
    {
      throw error(make<string>("not an number"));
    }
  }

  template <typename Tuple, auto I = 0, typename F>
  auto apply_to([[maybe_unused]] F f, object const& x, object const& y) -> object
  {
    if constexpr (I < std::tuple_size_v<Tuple>)
    {
      using type_i_0 = std::tuple_element_t<0, std::tuple_element_t<I, Tuple>>;
      using type_i_1 = std::tuple_element_t<1, std::tuple_element_t<I, Tuple>>;

      return x.is<type_i_0>() and y.is<type_i_1>() ? canonicalize(f(x.as<type_i_0>(), y.as<type_i_1>())) : apply_to<Tuple, I + 1>(f, x, y);
    }
    else
    {
      throw error(make<string>("not an number"));
    }
  }

  template <typename Tuple, auto I = 0, typename F>
  auto test([[maybe_unused]] F f, object const& x) -> bool
  {
    if constexpr (I < std::tuple_size_v<Tuple>)
    {
      using type_i = std::tuple_element_t<I, Tuple>;

      return x.is<type_i>() ? f(x.as<type_i>()) : test<Tuple, I + 1>(f, x);
    }
    else
    {
      return false;
    }
  }

  template <typename Tuple, auto I = 0, typename F>
  auto test([[maybe_unused]] F f, object const& x, object const& y) -> bool
  {
    if constexpr (I < std::tuple_size_v<Tuple>)
    {
      using type_i_0 = std::tuple_element_t<0, std::tuple_element_t<I, Tuple>>;
      using type_i_1 = std::tuple_element_t<1, std::tuple_element_t<I, Tuple>>;

      return x.is<type_i_0>() and y.is<type_i_1>() ? f(x.as<type_i_0>(), y.as<type_i_1>()) : test<Tuple, I + 1>(f, x, y);
    }
    else
    {
      return false;
    }
  }

  auto operator +(object const& x, object const& y) -> object
  {
    return apply_to<complex_numbers>(std::plus(), x, y);
  }

  auto operator -(object const& x, object const& y) -> object
  {
    return apply_to<complex_numbers>(std::minus(), x, y);
  }

  auto operator *(object const& x, object const& y) -> object
  {
    return apply_to<complex_numbers>(std::multiplies(), x, y);
  }

  auto operator /(object const& x, object const& y) -> object
  {
    return apply_to<complex_numbers>(std::divides(), x, y);
  }

  auto operator % (object const& x, object const& y) -> object
  {
    auto f = []<typename T, typename U>(T const& x, U const& y)
    {
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

    return apply_to<real_numbers>(f, x, y);
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
          return make(lexical_cast<double>(literal));
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

inline namespace number
{
  auto equals(object const& x, object const& y) -> bool
  {
    auto f = [](auto&&... xs)
    {
      return inexact_equals(std::forward<decltype(xs)>(xs)...);
    };

    return test<complex_numbers>(f, x, y);
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
        return std::is_same_v<T, exact_integer> or
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
        return x.denominator() == 1;
      }
      else
      {
        return std::is_same_v<T, exact_integer>;
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
        return std::is_same_v<T, ratio> or std::is_same_v<T, exact_integer>;
      }
    };

    return test<complex_number>(f, x);
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
    let static const e2 = make<exact_integer>(2);
    return is_zero(remainder(x, e2));
  }

  auto abs(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
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
      else if constexpr (std::is_same_v<T, complex>)
      {
        return square_root(x.real() * x.real() + x.imag() * x.imag());
      }
      else
      {
        static auto const zero = static_cast<exact_integer>(0);
        return x < zero ? zero - x : x;
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

  auto square_root(object const& x) -> object
  {
    auto f = []<typename T>(T const& x)
    {
      if constexpr (std::is_same_v<T, complex>)
      {
        auto const z = std::sqrt(static_cast<std::complex<double>>(x));

        return complex(make(z.real()),
                       make(z.imag()));
      }
      else
      {
        auto square_root = [](auto const& x)
        {
          if constexpr (std::is_same_v<T, exact_integer>)
          {
            auto const [s, r] = x.square_root();

            return r == 0 ? make(s) : make(std::sqrt(static_cast<double>(x)));
          }
          else
          {
            return make(std::sqrt(static_cast<double>(x)));
          }
        };

        return x < exact_integer(0) ? make<complex>(e0, square_root(exact_integer(0) - x))
                                    : square_root(x);
      }
    };

    return apply_to<complex_number>(f, x);
  }

  auto pow(object const& x, object const& y) -> object
  {
    auto f = []<typename T, typename U>(T const& x, U const& y)
    {
      if constexpr (std::is_same_v<T, complex> or
                    std::is_same_v<U, complex>)
      {
        auto inexact = [](auto&& x)
        {
          if constexpr (std::is_same_v<std::decay_t<decltype(x)>, complex>)
          {
            return static_cast<std::complex<double>>(std::forward<decltype(x)>(x));
          }
          else
          {
            return static_cast<double>(std::forward<decltype(x)>(x));
          }
        };

        auto const z = std::pow(inexact(std::forward<decltype(x)>(x)),
                                inexact(std::forward<decltype(y)>(y)));

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
        return std::pow(static_cast<double>(std::forward<decltype(x)>(x)),
                        static_cast<double>(std::forward<decltype(y)>(y)));
      }
    };

    return apply_to<complex_numbers>(f, x, y);
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

  auto load_exponent(object const& x, object const& y) -> object
  {
    auto f = [](auto&& x, auto&& y)
    {
      return std::ldexp(static_cast<double>(std::forward<decltype(x)>(x)),
                        static_cast<int   >(std::forward<decltype(y)>(y)));
    };

    return apply_to<real_numbers>(f, x, y);
  }

  auto number_to_string(object const& x, int radix) -> object
  {
    auto f = [radix]<typename T>(T const& x)
    {
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

    return apply_to<complex_number>(f, x);
  }

  #define DEFINE_EXACTNESS_PRESERVED_COMPLEX1(NAME, CMATH)                     \
  auto NAME(object const& x) -> object                                         \
  {                                                                            \
    auto f = []<typename T>(T const& x)                                        \
    {                                                                          \
      if constexpr (std::is_floating_point_v<T>)                               \
      {                                                                        \
        return CMATH(x);                                                       \
      }                                                                        \
      else if constexpr (std::is_same_v<T, ratio>)                             \
      {                                                                        \
        return exact_integer(CMATH(static_cast<double>(x)));                   \
      }                                                                        \
      else if constexpr (std::is_same_v<T, exact_integer>)                     \
      {                                                                        \
        return x;                                                              \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return complex(NAME(x.real()),                                         \
                       NAME(x.imag()));                                        \
      }                                                                        \
    };                                                                         \
                                                                               \
    return apply_to<complex_number>(f, x);                                     \
  }

  DEFINE_EXACTNESS_PRESERVED_COMPLEX1(ceiling,  std::ceil)
  DEFINE_EXACTNESS_PRESERVED_COMPLEX1(floor,    std::floor)
  DEFINE_EXACTNESS_PRESERVED_COMPLEX1(round,    std::round)
  DEFINE_EXACTNESS_PRESERVED_COMPLEX1(truncate, std::trunc)

  #define DEFINE_COMPLEX1(NAME, CMATH)                                         \
  auto NAME(object const& x) -> object                                         \
  {                                                                            \
    auto f = []<typename T>(T const& x)                                        \
    {                                                                          \
      if constexpr (std::is_same_v<T, complex>)                                \
      {                                                                        \
        auto const z = CMATH(static_cast<std::complex<double>>(std::forward<decltype(x)>(x))); \
                                                                               \
        return complex(make(z.real()),                                         \
                       make(z.imag()));                                        \
      }                                                                        \
      else                                                                     \
      {                                                                        \
        return CMATH(static_cast<double>(std::forward<decltype(x)>(x)));       \
      }                                                                        \
    };                                                                         \
                                                                               \
    return apply_to<complex_number>(f, x);                                     \
  }

  DEFINE_COMPLEX1(acos,  std::acos)
  DEFINE_COMPLEX1(acosh, std::acosh)
  DEFINE_COMPLEX1(asin,  std::asin)
  DEFINE_COMPLEX1(asinh, std::asinh)
  DEFINE_COMPLEX1(atan,  std::atan)
  DEFINE_COMPLEX1(atanh, std::atanh)
  DEFINE_COMPLEX1(cos,   std::cos)
  DEFINE_COMPLEX1(cosh,  std::cosh)
  DEFINE_COMPLEX1(exp,   std::exp)
  DEFINE_COMPLEX1(log,   std::log)
  DEFINE_COMPLEX1(sin,   std::sin)
  DEFINE_COMPLEX1(sinh,  std::sinh)
  DEFINE_COMPLEX1(tan,   std::tan)
  DEFINE_COMPLEX1(tanh,  std::tanh)

  #define DEFINE_REAL1(NAME, CMATH)                                            \
  auto NAME(object const& x) -> object                                         \
  {                                                                            \
    auto f = [](auto&& x)                                                      \
    {                                                                          \
      return CMATH(static_cast<double>(std::forward<decltype(x)>(x)));         \
    };                                                                         \
                                                                               \
    return apply_to<real_number>(f, x);                                        \
  }

  DEFINE_REAL1(gamma,     std::tgamma)
  DEFINE_REAL1(log_gamma, std::lgamma)

  #define DEFINE_REAL2(NAME, CMATH)                                            \
  auto NAME(object const& x, object const& y) -> object                        \
  {                                                                            \
    auto f = [](auto&& x, auto&& y)                                            \
    {                                                                          \
      return CMATH(static_cast<double>(std::forward<decltype(x)>(x)),          \
                   static_cast<double>(std::forward<decltype(y)>(y)));         \
    };                                                                         \
                                                                               \
    return apply_to<real_numbers>(f, x, y);                                    \
  }

  DEFINE_REAL2(atan,         std::atan2)
  DEFINE_REAL2(copy_sign,    std::copysign)
  DEFINE_REAL2(next_after,   std::nextafter)

  #if __cpp_lib_math_special_functions
  DEFINE_REAL2(cyl_bessel_j, std::cyl_bessel_j)
  DEFINE_REAL2(cyl_neumann,  std::cyl_neumann)
  #else
  auto cyl_bessel_j(object const&, object const&) -> object
  {
    throw error(make<string>("The mathematical special function std::cyl_bessel_j is not provided in this environment."));
  }

  auto cyl_neumann(object const&, object const&) -> object
  {
    throw error(make<string>("The mathematical special function std::cyl_neumann is not provided in this environment."));
  }
  #endif
} // namespace number
} // namespace meevax::kernel
