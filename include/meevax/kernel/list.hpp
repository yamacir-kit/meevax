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

#ifndef INCLUDED_MEEVAX_KERNEL_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_LIST_HPP

#include <meevax/kernel/comparator.hpp>
#include <meevax/kernel/number.hpp>

namespace meevax
{
inline namespace kernel
{
  inline auto list = [](auto&&... xs) constexpr
  {
    return (std::forward<decltype(xs)>(xs) | ... | nullptr);
  };

  auto make_list(std::size_t, object const& = nullptr) -> object;

  auto iota(std::size_t, object const& = e0, object const& = e1) -> object;

  template <typename T>
  auto last_pair(T&& x) -> decltype(x)
  {
    return cdr(x).template is<pair>() ? last_pair(cdr(std::forward<decltype(x)>(x))) : std::forward<decltype(x)>(x);
  }

  template <typename T>
  auto last(T&& x) -> decltype(x)
  {
    return car(last_pair(std::forward<decltype(x)>(x)));
  }

  template <typename T>
  auto circulate(T&& x)
  {
    cdr(last_pair(std::forward<decltype(x)>(x))) = x;
  }

  template <typename... Ts>
  auto circular_list(Ts&&... xs)
  {
    let x = list(std::forward<decltype(xs)>(xs)...);
    circulate(x);
    return x;
  }

  auto is_list(object const&) -> bool;

  auto is_circular_list(object const&) -> bool;

  auto is_dotted_list(object const&) -> bool;

  auto list_copy(object const&) -> object;

  template <typename T>
  auto tail(T&& x, std::size_t size) -> decltype(x)
  {
    return 0 < size ? tail(cdr(std::forward<decltype(x)>(x)), --size) : std::forward<decltype(x)>(x);
  }

  template <typename... Ts>
  auto head(Ts&&... xs) -> decltype(auto)
  {
    return car(tail(std::forward<decltype(xs)>(xs)...));
  }

  auto take(object const&, std::size_t) -> object;

  auto take(object &, std::size_t) -> object;

  auto take_right(object const&, std::size_t) -> object const&;

  auto drop(object const&, std::size_t) -> object const&;

  auto drop(object &, std::size_t) -> object &;

  auto drop_right(object const&, std::size_t) -> object;

  auto drop_right(object &, std::size_t) -> object;

  auto length(object const&) -> std::ptrdiff_t;

  auto append(object const&, object const&) -> object;

  auto append(object &, object const&) -> object &;

  auto append_reverse(object const&, object const&) -> object;

  auto append_reverse(object &, object const&) -> object;

  auto reverse(object const&) -> object;

  auto reverse(object &) -> object;

  template <typename F>
  auto map(F f, object const& xs) -> object
  {
    if (xs.is<pair>())
    {
      return cons(f(car(xs)), map(f, cdr(xs)));
    }
    else
    {
      return nullptr;
    }
  }

  auto memq(object const&, object const&) -> object const&;

  auto memv(object const&, object const&) -> object const&;

  auto assq(object const&, object const&) -> object const&;

  auto assv(object const&, object const&) -> object const&;

  auto alist_cons(object const&, object const&, object const&) -> object;

  auto alist_copy(object const&) -> object;

  template <typename F>
  auto filter(F test, object const& xs) -> object
  {
    if (xs.is<pair>())
    {
      if (test(car(xs)))
      {
        return cons(car(xs), filter(test, cdr(xs)));
      }
      else
      {
        return filter(test, cdr(xs));
      }
    }
    else
    {
      return nullptr;
    }
  }

  auto longest_common_tail(let const&, let const&) -> object const&;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIST_HPP
