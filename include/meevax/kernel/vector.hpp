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

#ifndef INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
#define INCLUDED_MEEVAX_KERNEL_VECTOR_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  enum class for_each_in_tag {} constexpr for_each_in {};

  struct vector
    : public std::vector<let>
  {
    using std::vector<let>::vector;

    template <typename InputIterator>
    explicit vector(for_each_in_tag, InputIterator from, InputIterator to)
    {
      std::copy(from, to, std::back_inserter(*this));
    }

    explicit vector(for_each_in_tag, let const&);

    auto fill(const_reference, size_type, size_type) -> void;

    auto fill(const_reference, size_type = 0) -> void;

    auto list(size_type, size_type) const -> value_type;

    auto list(size_type = 0) const -> value_type;

    auto string(size_type, size_type) const -> value_type;

    auto string(size_type = 0) const -> value_type;
  };

  auto operator ==(vector const&, vector const&) -> bool;

  auto operator <<(std::ostream &, vector const&) -> std::ostream &;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
