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

#ifndef INCLUDED_MEEVAX_KERNEL_RATIO_HPP
#define INCLUDED_MEEVAX_KERNEL_RATIO_HPP

#include <gmp.h>

#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct ratio : protected virtual pair
  {
    mpq_t value;

    ratio();

    ratio(ratio const&);

    explicit ratio(const_reference, const_reference);

    explicit ratio(double);

    explicit ratio(external_representation const&, int = 10);

    ~ratio();

    auto denominator() const -> const_reference;

    auto denominator() -> reference;

    auto invert() const -> ratio;

    auto numerator() const -> const_reference;

    auto numerator() -> reference;

    auto simple() const -> value_type;

    explicit operator double() const;
  };

  auto operator <<(std::ostream &, ratio const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
