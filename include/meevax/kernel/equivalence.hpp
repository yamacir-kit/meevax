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

#ifndef INCLUDED_MEEVAX_KERNEL_EQUIVALENCE_HPP
#define INCLUDED_MEEVAX_KERNEL_EQUIVALENCE_HPP

#include <meevax/kernel/boolean.hpp>

namespace meevax
{
inline namespace kernel
{
  auto eq = [](auto const& x, auto const& y) constexpr
  {
    return x == y;
  };

  auto eqv = [](auto const& x, auto const& y)
  {
    return x.eqv(y);
  };

  auto equal(pair::const_reference, pair::const_reference) -> bool;

  template <std::size_t Coarseness = 0>
  struct equivalence_comparator;

  #define SPECIALIZE_EQUIVALENCE_COMPARATOR(COARSENESS, COMPARE)               \
  template <>                                                                  \
  struct equivalence_comparator<COARSENESS>                                    \
  {                                                                            \
    template <typename... Ts>                                                  \
    constexpr auto operator ()(Ts&&... xs) const -> bool                       \
    {                                                                          \
      return COMPARE(std::forward<decltype(xs)>(xs)...);                       \
    }                                                                          \
  }

  SPECIALIZE_EQUIVALENCE_COMPARATOR(0, eq);
  SPECIALIZE_EQUIVALENCE_COMPARATOR(1, eqv);
  SPECIALIZE_EQUIVALENCE_COMPARATOR(2, equal);

  #undef SPECIALIZE_EQUIVALENCE_COMPARATOR

  using default_equivalence_comparator = equivalence_comparator<>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_EQUIVALENCE_HPP
