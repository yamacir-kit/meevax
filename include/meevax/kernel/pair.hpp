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

#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/object.hpp>

namespace meevax
{
inline namespace kernel
{
  let extern const unit;

  /* ---- Pair -----------------------------------------------------------------
   *
   *
   * ------------------------------------------------------------------------ */
  struct pair
    : public std::array<let, 2>
    , public top<pair>
  {
    explicit pair(let const& a = unit,
                  let const& b = unit)
      : std::array<let, 2> { a, b }
    {}

    virtual ~pair() = default;
  };

  auto operator <<(output_port & port, pair const&) -> output_port &;

  /* ---- Pair Accessor --------------------------------------------------------
   *
   *  Pair accessors are not only for pair type. Accessing car and cdr is a
   *  valid operation for everyone except the empty list.
   *
   * ------------------------------------------------------------------------ */
  auto car = [](auto&& x) noexcept -> decltype(auto) { return std::get<0>(unwrap(std::forward<decltype(x)>(x))); };
  auto cdr = [](auto&& x) noexcept -> decltype(auto) { return std::get<1>(unwrap(std::forward<decltype(x)>(x))); };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
