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

#ifndef INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP
#define INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP

#include <meevax/functional/combinator.hpp>
#include <meevax/functional/compose.hpp>
#include <meevax/iostream/concatenate.hpp>
#include <meevax/iostream/escape_sequence.hpp>
#include <meevax/iostream/lexical_cast.hpp>
#include <meevax/kernel/mnemonic.hpp>
#include <meevax/kernel/type_index.hpp>
#include <meevax/memory/gc_pointer.hpp>
#include <meevax/type_traits/is_equality_comparable.hpp>
#include <meevax/type_traits/is_output_streamable.hpp>
#include <meevax/type_traits/underlying_cast.hpp>
#include <meevax/utility/demangle.hpp>
#include <meevax/utility/module.hpp>

#define NIL /* nothing */

namespace meevax
{
inline namespace kernel
{
  struct pair;           // pair.hpp

  template <template <typename...> typename, typename, typename...>
  class heterogeneous;

  using value_type = heterogeneous<gc_pointer, pair, bool, std::int32_t, std::uint32_t, float, mnemonic>;

  using reference = value_type &;

  using const_reference = value_type const&;

  using let = value_type;

  using null = std::nullptr_t;

  [[noreturn]]
  auto raise(std::string const&) -> void; // error.hpp
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_OVERVIEW_HPP
