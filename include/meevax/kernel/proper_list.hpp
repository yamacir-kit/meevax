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

#include <meevax/kernel/comparator.hpp>

namespace meevax::inline kernel
{
  struct proper_list_view : public std::ranges::view_interface<proper_list_view>
  {
    object root;

    explicit proper_list_view(object const&);

    auto begin()
    {
      return pair::iterator(root.unsafe_get());
    }

    auto begin() const
    {
      return pair::const_iterator(root.unsafe_get());
    }

    auto end()
    {
      return pair::iterator(nullptr);
    }

    auto end() const
    {
      return pair::const_iterator(nullptr);
    }
  };

  struct proper_list_adaptor
  {
    auto operator ()(auto&& x) const
    {
      return proper_list_view(std::forward<decltype(x)>(x));
    }
  };

  auto inline constexpr as_proper_list = proper_list_adaptor();

  auto operator |(auto&& x, proper_list_adaptor const&) -> proper_list_view
  {
    return as_proper_list(std::forward<decltype(x)>(x));
  }

  auto length(proper_list_view const&) -> std::ptrdiff_t;
} // namespace meevax::kernel

namespace std::ranges
{
  template <>
  inline constexpr bool enable_borrowed_range<meevax::kernel::proper_list_view> = true;
}

#endif // INCLUDED_MEEVAX_KERNEL_PROPER_LIST_HPP

