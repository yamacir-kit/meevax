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

#include <meevax/kernel/complex.hpp>
#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  auto complex::real() const noexcept -> const_reference
  {
    return first;
  }

  auto complex::real() noexcept -> reference
  {
    return first;
  }

  auto complex::imag() const noexcept -> const_reference
  {
    return second;
  }

  auto complex::imag() noexcept -> reference
  {
    return second;
  }

  auto operator <<(std::ostream & os, complex const& z) -> std::ostream &
  {
    return os << z.real() << cyan(experimental::less.at(type_index<2>(e0.type(), z.imag().type()))(e0, z.imag()) ? '+' : '-') << z.imag() << cyan("i");
  }
} // namespace kernel
} // namespace meevax
