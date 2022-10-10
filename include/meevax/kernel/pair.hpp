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

#ifndef INCLUDED_MEEVAX_KERNEL_PAIR_HPP
#define INCLUDED_MEEVAX_KERNEL_PAIR_HPP

#include <meevax/kernel/object.hpp>

namespace meevax
{
inline namespace kernel
{
  let extern unit;

  struct pair : public std::pair<value_type, value_type>
              , public top<pair>
  {
    explicit pair(const_reference a = unit, const_reference b = unit)
      : std::pair<value_type, value_type> { a, b }
    {}

    template <typename... Ts, typename = std::enable_if_t<(1 < sizeof...(Ts))>>
    explicit pair(const_reference a, Ts&&... xs)
      : pair { a, make<pair>(std::forward<decltype(xs)>(xs)...) }
    {}

    virtual ~pair() = default;
  };

  auto operator <<(std::ostream &, pair const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
