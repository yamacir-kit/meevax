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

  using object = heterogeneous<gc_pointer, pair, bool, std::int32_t, std::uint32_t, float, mnemonic>;

  using let = object;

  let extern unit;

  template <typename T, typename... Ts>
  auto make(Ts&&... xs)
  {
    return object::allocate<T>(std::forward<decltype(xs)>(xs)...);
  }

  template <typename T>
  auto make(T&& x)
  {
    return object::allocate<std::decay_t<T>>(std::forward<decltype(x)>(x));
  }

  struct pair : public std::pair<object, object>
  {
    explicit pair(object const& = unit, object const& = unit);

    template <typename... Ts, typename = std::enable_if_t<(1 < sizeof...(Ts))>>
    explicit pair(object const& a, Ts&&... xs)
      : pair { a, make<pair>(std::forward<decltype(xs)>(xs)...) }
    {}

    virtual ~pair() = default;

    virtual auto compare(pair const*) const -> bool;

    virtual auto type() const noexcept -> std::type_info const&;

    virtual auto write(std::ostream &) const -> std::ostream &;

    virtual auto operator [](std::size_t) const -> object const&;
  };

  auto operator <<(std::ostream &, pair const&) -> std::ostream &;

  auto write_simple(std::ostream &, pair const&) -> std::ostream &;

  auto write_simple(std::ostream &, object const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_PAIR_HPP
