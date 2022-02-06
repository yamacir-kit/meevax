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

#ifndef INCLUDED_MEEVAX_KERNEL_ENVIRONMENT_HPP
#define INCLUDED_MEEVAX_KERNEL_ENVIRONMENT_HPP

#include <meevax/kernel/configurator.hpp>
#include <meevax/kernel/machine.hpp>
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/writer.hpp>
#include <meevax/utility/integer_sequence.hpp>

namespace meevax
{
inline namespace kernel
{
  class environment : public virtual pair
                    , public configurator<environment>
                    , public machine     <environment>
                    , public reader      <environment>
                    , public writer      <environment>
  {
    using pair::pair;

  public:
    using configurator::is_debug_mode;
    using configurator::is_trace_mode;
    using configurator::is_verbose_mode;

    using reader::intern;
    using reader::read;

    using writer::print;
    using writer::debug_port;
    using writer::write;

    explicit environment(environment &&) = default;

    explicit environment(environment const&) = default;

    template <typename... Ts, REQUIRES(is_integer_sequence<Ts>...)>
    explicit environment(Ts&&... xs)
    {
      import(), (import(xs), ...);
    }

    auto operator [](const_reference) -> const_reference;

    auto operator [](std::string const&) -> const_reference;

    auto define(const_reference, const_reference) -> const_reference;

    auto define(std::string const&, const_reference) -> const_reference;

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> const_reference
    {
      return define(intern(name), make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    auto evaluate(const_reference) -> object;

    auto execute() -> object;

    auto global() noexcept -> reference;

    auto global() const noexcept -> const_reference;

    template <typename T, T... xs>
    auto import(std::integer_sequence<T, xs...>) -> void;

    auto import() -> void;

    auto load(std::string const&) -> object;

    auto load(const_reference) -> object;

    auto rename(const_reference) -> const_reference;

    auto rename(const_reference, const_reference) -> object;

    auto rename(const_reference) const -> const_reference;

    auto rename(const_reference, const_reference) const -> object;
  };

  auto operator >>(std::istream &, environment &) -> std::istream &;

  auto operator <<(std::ostream &, environment &) -> std::ostream &;

  auto operator <<(std::ostream &, environment const&) -> std::ostream &;

  extern template class configurator<environment>;

  extern template class machine<environment>;

  extern template class reader<environment>;

  extern template class writer<environment>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ENVIRONMENT_HPP
