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

namespace meevax
{
inline namespace kernel
{
  class environment : public virtual pair
                    , public configurator<environment>
                    , public machine<environment>
                    , public optimizer
                    , public reader<environment>
  {
    using pair::pair;

  public:
    using configurator::debug;
    using configurator::trace;

    using reader::read;

    using machine::quote;

    environment(environment &&) = default;

    environment(environment const&) = default;

    auto operator [](object const& variable) -> decltype(auto)
    {
      return identify(variable).as<identity>().load(e);
    }

    auto operator [](std::string const& variable) -> decltype(auto)
    {
      return (*this)[string_to_symbol(variable)];
    }

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

    auto execute()              -> object;
    auto execute(object const&) -> object;

    auto fork()              const -> object;
    auto fork(object const&) const -> object;

    auto global() const noexcept -> object const&;
    auto global()       noexcept -> object      &;

    auto load(std::string const&) -> object;

    auto scope() const noexcept -> object const&;
    auto scope()       noexcept -> object      &;

    auto identify(object const&, object const&) const -> object;
    auto identify(object const&)                const -> object;
    auto identify(object const&, object const&)       -> object;
    auto identify(object const&)                      -> object;
  };

  auto operator >>(std::istream &, environment &) -> std::istream &;

  auto operator <<(std::ostream &, environment &) -> std::ostream &;

  auto operator <<(std::ostream &, environment const&) -> std::ostream &;

  extern template class configurator<environment>;

  extern template class machine<environment>;

  extern template class reader<environment>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ENVIRONMENT_HPP
