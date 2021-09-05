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

#ifndef INCLUDED_MEEVAX_KERNEL_DE_BRUIJN_INDEX_HPP
#define INCLUDED_MEEVAX_KERNEL_DE_BRUIJN_INDEX_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Comparator = default_equivalence_comparator>
  struct de_bruijn_index
  {
    Comparator compare {};

    bool is_variadic;

    let const index;

    template <typename... Ts>
    explicit de_bruijn_index(Ts&&... xs)
      : index { notate(std::forward<decltype(xs)>(xs)...) }
    {}

    auto notate(pair::const_reference value, pair::const_reference frames) -> pair::value_type // XXX UGLY CODE!!!
    {
      std::size_t layer = 0;

      for (auto const& frame : frames)
      {
        std::size_t index = 0;

        for (let node = frame; node; node = cdr(node))
        {
          if (node.is<pair>() and compare(car(node), value))
          {
            is_variadic = false;
            return cons(make<exact_integer>(layer), make<exact_integer>(index));
          }
          else if (node.is<symbol>() and compare(node, value))
          {
            is_variadic = true;
            return cons(make<exact_integer>(layer), make<exact_integer>(index));
          }

          ++index;
        }

        ++layer;
      }

      return unit;
    }

    auto is_bound() const -> bool
    {
      return not index.is<null>();
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_DE_BRUIJN_INDEX_HPP
