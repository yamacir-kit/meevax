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

#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/identifier.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>
#include <meevax/posix/vt10x.hpp>

namespace meevax
{
inline namespace kernel
{
  auto identifier::symbol() const -> const_reference
  {
    return car(*this);
  }

  auto operator <<(std::ostream & os, identifier const& datum) -> std::ostream &
  {
    return os << underline << datum.symbol() << reset;
  }

  auto absolute::is_bound() const -> bool
  {
    return not is_free();
  }

  auto absolute::is_free() const -> bool
  {
    return cdr(*this).is<absolute>() and cdr(*this).as<absolute>() == *this; // NOTE: See syntactic_continuation::locate
  }

  auto relative::is_bound() const -> bool
  {
    return not is_free();
  }

  auto relative::is_free() const -> bool
  {
    return false;
  }

  auto notate(pair::const_reference variable, pair::const_reference frames) -> pair::value_type
  {
    for (auto outer = std::begin(frames); outer != std::end(frames); ++outer)
    {
      for (auto inner = std::begin(*outer); inner != std::end(*outer); ++inner)
      {
        if (inner.unwrap().is<pair>() and eq(*inner, variable))
        {
          return make<relative>(variable,
                                cons(make<exact_integer>(std::distance(std::begin(frames), outer)),
                                     make<exact_integer>(std::distance(std::begin(*outer), inner))));
        }
        else if (inner.unwrap().is<symbol>() and eq(inner, variable))
        {
          return make<variadic>(variable,
                                cons(make<exact_integer>(std::distance(std::begin(frames), outer)),
                                     make<exact_integer>(std::distance(std::begin(*outer), inner))));
        }
      }
    }

    return unit; // TODO call syntactic_continuation::locate
  }
} // namespace kernel
} // namespace meevax
