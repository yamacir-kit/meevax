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

#ifndef INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

#include <meevax/kernel/proper_list.hpp>

namespace meevax::inline kernel
{
  struct heterogeneous_vector
  {
    std::vector<object> objects;

    explicit heterogeneous_vector(auto&&... xs)
      : objects { std::forward<decltype(xs)>(xs)... }
    {}

    template <typename Pair>
    explicit heterogeneous_vector(proper_list_view<Pair> view)
    {
      std::ranges::copy(view, std::back_inserter(objects));
    }
  };

  auto operator ==(heterogeneous_vector const&, heterogeneous_vector const&) -> bool;

  auto operator <<(std::ostream &, heterogeneous_vector const&) -> std::ostream &;

  using vector = heterogeneous_vector;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
