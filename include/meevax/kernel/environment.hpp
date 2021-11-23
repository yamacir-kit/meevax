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
    /* ---- NOTE ---------------------------------------------------------------
     *
     *  If this class is constructed as make<environment>(...) then the
     *  heterogeneous::binder will have forwarded all constructor arguments to
     *  the virtual base class pair in advance, and this constructor will be
     *  called without any arguments.
     *
     *  (See the heterogeneous::binder::binder for details)
     *
     * ---------------------------------------------------------------------- */
    using pair::pair;

  public:
    let datum = unit;

    using configurator::is_debug_mode;
    using configurator::is_trace_mode;
    using configurator::is_verbose_mode;

    using reader::intern;
    using reader::read;

    using writer::print;
    using writer::debug_port;
    using writer::write;

    template <typename... Ts>
    explicit environment(Ts&&... xs)
    {
      import(), (import(xs), ...);
    }

    auto operator [](const_reference) -> const_reference;

    auto operator [](std::string const&) -> const_reference;

    auto build(continuation const&) -> void; // NOTE: Only fork() may call this function.

    auto current_expression() const -> const_reference;

    auto define(const_reference, const_reference) -> const_reference;

    auto define(std::string const&, const_reference) -> const_reference;

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> const_reference
    {
      return define(intern(name), make<T>(name, std::forward<decltype(xs)>(xs)...));
    }

    auto dynamic_environment() const -> const_reference;

    auto evaluate(const_reference) -> object;

    auto execute() -> object;

    auto form()       noexcept ->       reference;
    auto form() const noexcept -> const_reference;

    auto global()       noexcept ->       reference;
    auto global() const noexcept -> const_reference;

    auto import() -> void;

    template <typename T>
    auto import(T) -> void;

    auto load(std::string const&) -> object;

    auto load(const_reference) -> object;

    auto rename(const_reference) -> const_reference;

    auto rename(const_reference, const_reference) -> object;

    auto rename(const_reference) const -> const_reference;

    auto rename(const_reference, const_reference) const -> object;
  };

  auto operator >>(std::istream &, environment &) -> std::istream &;

  auto operator <<(std::ostream &, environment      &) -> std::ostream &;
  auto operator <<(std::ostream &, environment const&) -> std::ostream &;

  extern template class configurator<environment>;

  extern template class machine<environment>;

  extern template class reader<environment>;

  extern template class writer<environment>;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_ENVIRONMENT_HPP
