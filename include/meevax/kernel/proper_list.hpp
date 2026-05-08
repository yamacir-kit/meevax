/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_PROPER_LIST_HPP
#define INCLUDED_MEEVAX_KERNEL_PROPER_LIST_HPP

#include <meevax/kernel/list.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  template <typename Pair>
  struct proper_list_view : public std::ranges::view_interface<proper_list_view<Pair>>
  {
    Pair * p;

    explicit constexpr proper_list_view(Pair * p)
      : p { p }
    {}

    auto begin()
    {
      return std::conditional_t<std::is_const_v<Pair>, pair::const_iterator, pair::iterator>(p);
    }

    auto begin() const
    {
      return pair::const_iterator(p);
    }

    auto end()
    {
      return std::default_sentinel_t();
    }

    auto end() const
    {
      return std::default_sentinel_t();
    }
  };

  struct proper_list_adaptor
  {
    auto operator ()(auto&& x) const
    {
      assert(is_list(x));
      return proper_list_view(x.get());
    }
  };

  auto inline constexpr as_proper_list = proper_list_adaptor();

  auto operator |(auto&& x, proper_list_adaptor const&)
  {
    return as_proper_list(std::forward<decltype(x)>(x));
  }
} // namespace meevax::kernel

namespace std::ranges
{
  template <typename Pair>
  inline constexpr bool enable_borrowed_range<meevax::kernel::proper_list_view<Pair>> = true;
}

#endif // INCLUDED_MEEVAX_KERNEL_PROPER_LIST_HPP
