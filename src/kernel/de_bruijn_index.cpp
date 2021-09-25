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

#include <meevax/kernel/de_bruijn_index.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  de_bruijn_index::de_bruijn_index(pair::const_reference variable, pair::const_reference frames)
    : index { notate(variable, frames) }
  {}

  // XXX UGLY CODE!!!
  auto de_bruijn_index::notate(pair::const_reference variable, pair::const_reference frames) -> pair::value_type
  {
    std::size_t layer = 0;

    for (auto const& frame : frames)
    {
      std::size_t index = 0;

      for (let node = frame; node; node = cdr(node))
      {
        if (node.is<pair>() and eq(car(node), variable))
        {
          is_variadic = false;
          return cons(make<exact_integer>(layer), make<exact_integer>(index));
        }
        else if (node.is<symbol>() and eq(node, variable))
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

  auto de_bruijn_index::is_bound() const -> bool
  {
    return not is_free();
  }

  auto de_bruijn_index::is_free() const -> bool
  {
    return index.is<null>();
  }
} // namespace kernel
} // namespace meevax
