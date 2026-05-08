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

#ifndef INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP
#define INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP

#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  struct proper_list_policy
  {
    auto static constexpr circular_list_detection = false;

    auto static constexpr dotted_list_detection = false;
  };

  struct circular_list_policy
  {
    auto static constexpr circular_list_detection = true;

    auto static constexpr dotted_list_detection = false;
  };

  struct dotted_list_policy
  {
    auto static constexpr circular_list_detection = false;

    auto static constexpr dotted_list_detection = true;
  };

  struct unknown_list_policy
  {
    auto static constexpr circular_list_detection = true;

    auto static constexpr dotted_list_detection = true;
  };

  template <typename Pair, typename Policy>
  struct forward_list_iterator
  {
    using iterator_category = std::forward_iterator_tag;

    using value_type = Pair::first_type;

    using reference = std::add_lvalue_reference_t<std::conditional_t<std::is_const_v<Pair>, std::add_const_t<value_type>, value_type>>;

    using pointer = std::add_pointer_t<std::remove_reference_t<reference>>;

    using difference_type = std::ptrdiff_t;

    using size_type = std::size_t;

    Pair * node = nullptr;

    Pair * root = nullptr;

    forward_list_iterator() = default;

    explicit constexpr forward_list_iterator(Pair * root)
      : node { root }
      , root { root }
    {}

    auto operator *() const -> reference
    {
      return node->first;
    }

    auto operator ->() const -> pointer
    {
      return &node->first;
    }

    auto operator ++() -> decltype(auto)
    {
      assert(node);

      node = node->second.get();

      if constexpr (Policy::circular_list_detection)
      {
        if (node == root)
        {
          node = nullptr;
        }
      }

      if constexpr (Policy::dotted_list_detection)
      {
        if (node and node->type() != typeid(std::remove_const_t<Pair>)) // The node is neither null nor a pair.
        {
          node = nullptr;
        }
      }

      return *this;
    }

    auto operator ++(int) -> decltype(auto)
    {
      auto copy = *this;
      operator ++();
      return copy;
    }

    auto friend constexpr operator ==(forward_list_iterator const& a, forward_list_iterator const& b) noexcept
    {
      return a.node == b.node;
    }

    auto friend constexpr operator !=(forward_list_iterator const& a, forward_list_iterator const& b) noexcept
    {
      return a.node != b.node;
    }

    auto friend constexpr operator ==(forward_list_iterator const& a, std::default_sentinel_t)
    {
      return a.node == nullptr;
    }

    auto friend constexpr operator !=(forward_list_iterator const& a, std::default_sentinel_t)
    {
      return a.node != nullptr;
    }

    auto friend constexpr operator ==(std::default_sentinel_t, forward_list_iterator const& b)
    {
      return b.node == nullptr;
    }

    auto friend constexpr operator !=(std::default_sentinel_t, forward_list_iterator const& b)
    {
      return b.node != nullptr;
    }
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_ITERATOR_HPP
