/*
   Copyright 2018-2024 Tatsuya Yamasaki.

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

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  struct heterogeneous_vector
  {
    std::vector<object> vector;

    explicit heterogeneous_vector() = default;

    explicit heterogeneous_vector(object const&);

    explicit heterogeneous_vector(std::size_t, object const&);

    template <typename Iterator>
    explicit heterogeneous_vector(Iterator begin, Iterator end)
      : vector { begin, end }
    {}
  };

  auto operator ==(heterogeneous_vector const&, heterogeneous_vector const&) -> bool;

  auto operator <<(std::ostream &, heterogeneous_vector const&) -> std::ostream &;

  using vector = heterogeneous_vector;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
