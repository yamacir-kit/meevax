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
#include <meevax/kernel/machine.hpp>
#include <meevax/kernel/optimizer.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/writer.hpp>

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
    using reader::string_to_symbol;

    explicit environment(environment &&) = default;

    explicit environment(environment const&) = default;

    template <typename... Ts, REQUIRES(std::is_convertible<Ts, std::string>...)>
    explicit environment(Ts&&... xs)
    {
      (import(xs), ...);
    }

    auto operator [](const_reference variable) -> decltype(auto)
    {
      return identify(variable, scope()).as<identity>().load(e);
    }

    auto operator [](symbol::value_type const& variable) -> decltype(auto)
    {
      return (*this)[string_to_symbol(variable)];
    }

    auto define(const_reference, const_reference = undefined) -> void;

    auto define(symbol::value_type const&, const_reference = undefined) -> void;

    template <typename T, typename... Ts>
    auto define(symbol::value_type const& name, Ts&&... xs) -> void
    {
      define(name, make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    auto evaluate(const_reference) -> value_type;

    auto execute() -> value_type;

    auto execute(const_reference) -> value_type;

    auto fork() const -> value_type;

    auto fork(const_reference) const -> value_type;

    auto global() noexcept -> reference;

    auto global() const noexcept -> const_reference;

    auto import_(const_reference) -> void;

    auto import_(std::string const&) -> void;

    auto load(std::string const&) -> value_type;

    auto resolve(const_reference) -> value_type;

    auto scope() const noexcept -> const_reference;

    auto scope() noexcept -> reference;

    auto identify(const_reference, const_reference) -> value_type;

    auto identify(const_reference, const_reference) const -> value_type;
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
