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

#ifndef INCLUDED_MEEVAX_KERNEL_DE_BRUJIN_INDEX_HPP
#define INCLUDED_MEEVAX_KERNEL_DE_BRUJIN_INDEX_HPP

#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Comparator = default_equivalence_comparator>
  class de_bruijn_index : public let
  {
    bool variadic;

  public:
    Comparator compare {};

    template <typename... Ts>
    explicit de_bruijn_index(Ts&&... xs)
      : let { lookup(std::forward<decltype(xs)>(xs)...) }
    {}

    let lookup(let const& value, let const& frames)
    {
      std::size_t layer = 0;

      for (auto const& frame : frames)
      {
        std::size_t index = 0;

        for (auto iter = std::begin(frame); iter != std::end(frame); ++iter)
        {
          if (static_cast<let const&>(iter).is<pair>() and compare(*iter, value))
          {
            variadic = false;
            return cons(make<exact_integer>(layer), make<exact_integer>(index));
          }
          else if (static_cast<let const&>(iter).is<symbol>() and compare(iter, value))
          {
            variadic = true;
            return cons(make<exact_integer>(layer), make<exact_integer>(index));
          }

          ++index;
        }

        ++layer;
      }

      return make<pair>();
    }

    auto is_bound() const
    {
      return car(static_cast<let const&>(*this)) and
             cdr(static_cast<let const&>(*this));
    }

    auto is_variadic() const noexcept -> bool
    {
      return variadic;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_DE_BRUJIN_INDEX_HPP
