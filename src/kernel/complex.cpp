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

#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/number.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto complex::real() const noexcept -> complex::const_reference
  {
    return car(*this);
  }

  auto complex::real() noexcept -> complex::reference
  {
    return const_cast<complex::reference>(std::as_const(*this).real());
  }

  auto complex::imag() const noexcept -> complex::const_reference
  {
    return cdr(*this);
  }

  auto complex::imag() noexcept -> complex::reference
  {
    return const_cast<complex::reference>(std::as_const(*this).imag());
  }

  auto operator <<(std::ostream & os, complex const& z) -> std::ostream &
  {
    return os << cyan << z.real() << (e0 < z.imag() ? '+' : '-') << z.imag() << "i" << reset;
  }
} // namespace kernel
} // namespace meevax
