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

#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/identifier.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  auto notate(const_reference variable, const_reference frames) -> object
  {
    for (auto outer = std::begin(frames); outer != std::end(frames); ++outer)
    {
      for (auto inner = std::begin(*outer); inner != std::end(*outer); ++inner)
      {
        if (inner.is<pair>() and (*inner).is<keyword>() and eq((*inner).as<keyword>().symbol(), variable))
        {
          return *inner;
        }
        else if (inner.is<pair>() and eq(*inner, variable))
        {
          return make<relative>(variable,
                                cons(make<exact_integer>(std::distance(std::begin(frames), outer)),
                                     make<exact_integer>(std::distance(std::begin(*outer), inner))));
        }
        else if (inner.is<symbol>() and eq(inner, variable))
        {
          return make<variadic>(variable,
                                cons(make<exact_integer>(std::distance(std::begin(frames), outer)),
                                     make<exact_integer>(std::distance(std::begin(*outer), inner))));
        }
      }
    }

    return f;
  }
} // namespace kernel
} // namespace meevax
