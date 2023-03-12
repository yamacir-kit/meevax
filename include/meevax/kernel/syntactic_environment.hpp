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

#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP

#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/list.hpp>

namespace meevax
{
inline namespace kernel
{
  template <typename Environment>
  struct syntactic_environment : public virtual pair // (<local> . <global>)
  {
    using pair::pair;

    auto global() const noexcept -> object const&
    {
      return second;
    }

    auto global() noexcept -> object &
    {
      return second;
    }

    auto identify(object const& variable, object const& scope) const -> object
    {
      if (not variable.is_also<identifier>())
      {
        return f;
      }
      else
      {
        for (auto outer = std::begin(scope); outer != std::end(scope); ++outer)
        {
          for (auto inner = std::begin(*outer); inner != std::end(*outer); ++inner)
          {
            if (inner.get().is<pair>() and (*inner).is<absolute>() and eq((*inner).as<absolute>().symbol(), variable))
            {
              return *inner;
            }
            else if (inner.get().is<pair>() and eq(*inner, variable))
            {
              return make<relative>(make(static_cast<identity::index>(std::distance(std::begin(scope), outer))),
                                    make(static_cast<identity::index>(std::distance(std::begin(*outer), inner))));
            }
            else if (inner.get().is<symbol>() and eq(inner, variable))
            {
              return make<variadic>(make(static_cast<identity::index>(std::distance(std::begin(scope), outer))),
                                    make(static_cast<identity::index>(std::distance(std::begin(*outer), inner))));
            }
          }
        }

        if (variable.is<typename Environment::syntactic_closure>())
        {
          return variable.as<typename Environment::syntactic_closure>().identify();
        }
        else
        {
          return assq(variable, global());
        }
      }
    }

    auto identify(object const& variable, object const& scope)
    {
      if (not variable.is_also<identifier>())
      {
        return f;
      }
      else if (let const& identity = std::as_const(*this).identify(variable, scope); is_truthy(identity))
      {
        return identity;
      }
      else /* --------------------------------------------------------------------
      *
      *  At the outermost level of a program, a definition
      *
      *      (define <variable> <expression>)
      *
      *  has essentially the same effect as the assignment expression
      *
      *      (set! <variable> <expression>)
      *
      *  if <variable> is bound to a non-syntax value. However, if <variable> is
      *  not bound, or is a syntactic keyword, then the definition will bind
      *  <variable> to a new location before performing the assignment, whereas
      *  it would be an error to perform a set! on an unbound variable.
      *
      * ----------------------------------------------------------------------- */
      {
        return car(global() = make<absolute>(variable, undefined) | global());
      }
    }

    auto scope() const noexcept -> object const&
    {
      return first;
    }

    auto scope() noexcept -> object &
    {
      return first;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP
