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

#ifndef INCLUDED_MEEVAX_KERNEL_ENVIRONMENT_HPP
#define INCLUDED_MEEVAX_KERNEL_ENVIRONMENT_HPP

#include <meevax/kernel/configurator.hpp>
#include <meevax/kernel/import_set.hpp>
#include <meevax/kernel/machine.hpp>
#include <meevax/kernel/optimizer.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/syntactic_environment.hpp>

namespace meevax
{
inline namespace kernel
{
  class environment : public syntactic_environment
                    , public configurator<environment>
                    , public machine<environment>
                    , public optimizer
                    , public reader<environment>
  {
    using syntactic_environment::syntactic_environment;

  public:
    environment(environment &&) = default;

    environment(environment const&) = default;

    template <typename T, typename... Ts>
    auto declare(Ts&&... xs) -> decltype(auto)
    {
      return std::decay_t<T>(std::forward<decltype(xs)>(xs)...).resolve(*this);
    }

    auto define(object const&, object const& = undefined) -> void;

    auto define(std::string const&, object const& = undefined) -> void;

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> void
    {
      define(name, make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    auto evaluate(object const&) -> object;

    auto fork()              const -> object;
    auto fork(object const&) const -> object;

    auto load(std::string const&) -> object;

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

        if (variable.is<syntactic_closure>())
        {
          return variable.as<syntactic_closure>().identify();
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

    auto operator [](object const&) -> object const&;

    auto operator [](std::string const&) -> object const&;
  };

  auto operator <<(std::ostream &, environment const&) -> std::ostream &;

  extern template class configurator<environment>;

  extern template class machine<environment>;

  extern template class reader<environment>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ENVIRONMENT_HPP
