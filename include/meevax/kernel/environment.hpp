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
#include <meevax/kernel/reader.hpp>
#include <meevax/kernel/writer.hpp>
#include <meevax/utility/integer_sequence.hpp>

namespace meevax
{
inline namespace kernel
{
  inline namespace experimental
  {
    struct master_t
    {
      explicit master_t() = default;
    } constexpr master;
  } // namespace experimental

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

    explicit environment(master_t);

    template <typename... Ts, REQUIRES(std::is_convertible<Ts, std::string>...)>
    explicit environment(Ts&&... xs)
      : environment { master }
    {
      auto import = [](auto&& x)
      {
        std::cout << x << std::endl;
      };

      (import(xs), ...);

      define<procedure>("set-batch!",       [this](let const& xs, auto&&...) { return batch       = car(xs); });
      define<procedure>("set-debug!",       [this](let const& xs, auto&&...) { return debug       = car(xs); });
      define<procedure>("set-interactive!", [this](let const& xs, auto&&...) { return interactive = car(xs); });
      define<procedure>("set-prompt!",      [this](let const& xs, auto&&...) { return prompt      = car(xs); });
      define<procedure>("set-trace!",       [this](let const& xs, auto&&...) { return trace       = car(xs); });
      define<procedure>("set-verbose!",     [this](let const& xs, auto&&...) { return verbose     = car(xs); });
    }

    auto operator [](const_reference) -> const_reference;

    auto operator [](std::string const&) -> const_reference;

    auto apply(const_reference, const_reference) -> object;

    auto define(const_reference, const_reference = undefined) -> void;

    auto define(std::string const&, const_reference = undefined) -> void;

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> void
    {
      define(name, make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    auto evaluate(const_reference) -> object;

    auto execute() -> object;

    auto execute(const_reference) -> object;

    auto fork() const -> object
    {
      return make<environment>(*this);
    }

    auto fork(const_reference scope) const // DIRTY HACK!!!
    {
      let const copy = make<environment>(*this);
      copy.as<environment>().scope() = scope;
      return copy;
    }

    auto global() noexcept -> reference;

    auto global() const noexcept -> const_reference;

    auto load(std::string const&) -> object;

    auto scope() const noexcept -> const_reference;

    auto scope() noexcept -> reference;

    auto identify(const_reference, const_reference) -> object;

    auto identify(const_reference, const_reference) const -> object;
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
