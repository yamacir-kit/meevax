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

#ifndef INCLUDED_MEEVAX_KERNEL_COMPLEX_HPP
#define INCLUDED_MEEVAX_KERNEL_COMPLEX_HPP

#include <complex>

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct complex : public virtual pair
  {
    using pair::pair;

    explicit complex(std::string const&, int = 10);

    auto canonicalize() const -> object;

    auto imag() const noexcept -> object const&;

    auto real() const noexcept -> object const&;

    explicit operator std::complex<double>();
  };

  auto operator <<(std::ostream &, complex const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_COMPLEX_HPP
