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

#include <meevax/kernel/heterogeneous.hpp>

namespace meevax
{
inline namespace kernel
{
  struct pair;

  using value_type = heterogeneous<gc_pointer, pair, bool, std::int32_t, std::uint32_t, float, mnemonic>;

  using reference = value_type &;

  using const_reference = value_type const&;

  using let = value_type;

  let extern unit;

  template <typename T, typename... Ts>
  auto make(Ts&&... xs)
  {
    return value_type::allocate<T>(std::forward<decltype(xs)>(xs)...); // NOTE: This leaks memory if exception thrown from T's constructor.
  }

  template <typename T>
  auto make(T&& x)
  {
    return value_type::allocate<std::decay_t<T>>(std::forward<decltype(x)>(x));
  }

  struct pair : public std::pair<value_type, value_type>
  {
    explicit pair(const_reference = unit, const_reference = unit);

    template <typename... Ts, typename = std::enable_if_t<(1 < sizeof...(Ts))>>
    explicit pair(const_reference a, Ts&&... xs)
      : pair { a, make<pair>(std::forward<decltype(xs)>(xs)...) }
    {}

    virtual ~pair() = default;

    virtual auto compare(pair const*) const -> bool;

    virtual auto type() const noexcept -> std::type_info const&;

    virtual auto write(std::ostream &) const -> std::ostream &;
  };

  auto operator <<(std::ostream &, pair const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
