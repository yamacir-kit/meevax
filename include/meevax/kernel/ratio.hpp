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

#ifndef INCLUDED_MEEVAX_KERNEL_RATIO_HPP
#define INCLUDED_MEEVAX_KERNEL_RATIO_HPP

#include <gmp.h>
#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  struct ratio
  {
    mpq_t value;

    ratio();

    ratio(ratio const&);

    ratio(ratio &&);

    ~ratio();

    explicit ratio(std::int64_t);

    explicit ratio(exact_integer const&);

    explicit ratio(exact_integer const&, exact_integer const&);

    explicit ratio(double);

    explicit ratio(std::string const&, int = 10);

    auto denominator() const -> exact_integer;

    auto numerator() const -> exact_integer;

    template <typename T>
    explicit operator T() const
    {
      return mpq_get_d(value);
    }
  };

  auto operator <<(std::ostream &, ratio const&) -> std::ostream &;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
