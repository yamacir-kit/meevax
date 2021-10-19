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

  template <typename T>
  using pair_of = std::pair<T, T>;

  struct pair : public pair_of<object>
              , public top<pair>
  {
    using value_type = object;

    using       reference = let      &;
    using const_reference = let const&;

    explicit pair(const_reference a = unit, const_reference b = unit)
      : pair_of<object> { a, b }
    {}

    template <typename... Ts, typename = typename std::enable_if<std::less()(1, sizeof...(Ts))>::type>
    explicit pair(const_reference a, Ts&&... xs)
      : pair_of<object> { a, make<pair>(std::forward<decltype(xs)>(xs)...) }
    {}

    virtual ~pair() = default;
  };

  auto operator <<(std::ostream &, pair const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
