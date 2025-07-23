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

#include <meevax/kernel/list.hpp>

namespace meevax::inline kernel
{
  struct heterogeneous_vector : private std::vector<object>
  {
    using std::vector<object>::begin;
    using std::vector<object>::clear;
    using std::vector<object>::end;
    using std::vector<object>::insert;
    using std::vector<object>::operator [];
    using std::vector<object>::push_back;
    using std::vector<object>::rbegin;
    using std::vector<object>::rend;
    using std::vector<object>::reserve;
    using std::vector<object>::size;
    using std::vector<object>::value_type;
    using std::vector<object>::vector;
  };

  auto operator ==(heterogeneous_vector const&, heterogeneous_vector const&) -> bool;

  auto operator <<(std::ostream &, heterogeneous_vector const&) -> std::ostream &;

  using vector = heterogeneous_vector;

  auto make_vector(object const&) -> object; // list->vector
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_VECTOR_HPP
