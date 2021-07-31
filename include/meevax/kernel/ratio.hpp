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

#ifndef INCLUDED_MEEVAX_KERNEL_RATIO_HPP
#define INCLUDED_MEEVAX_KERNEL_RATIO_HPP

#include <meevax/kernel/numeric_tower.hpp>

namespace meevax
{
inline namespace kernel
{
  struct ratio
    : public virtual pair
  {
    using pair::pair;

    auto numerator() const -> exact_integer const&;

    auto denominator() const -> exact_integer const&;

    auto is_integer() const -> bool;

    auto invert() const -> ratio;

    auto reduce() const -> ratio;

    auto as_exact() const noexcept -> ratio const&
    {
      return *this;
    }

    template <typename T>
    auto as_inexact() const
    {
      return floating_point(numerator().as_inexact<T>() / denominator().as_inexact<T>());
    }
  };

  auto operator <<(output_port & port, ratio const&) -> output_port &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_RATIO_HPP
