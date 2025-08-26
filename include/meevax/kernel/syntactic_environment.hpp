/*
   Copyright 2018-2025 Tatsuya Yamasaki.

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

#include <meevax/kernel/describable.hpp>
#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/syntactic_closure.hpp>

namespace meevax::inline kernel
{
  struct syntactic_environment : public virtual pair // (<bound-variables> . <free-variables>)
  {
    struct syntax : public describable
    {
      auto (*expand)(syntactic_environment const&,
                     object const& form,
                     object const& bound_variables,
                     typename syntactic_closure::renamer &) -> object;

      auto (*generate)(syntactic_environment &,
                       object const& /* form            */,
                       object const& /* bound_variables */,
                       object const& /* continuation    */,
                       bool          /* tail            */) -> object;

      template <typename Expander, typename Generator>
      explicit syntax(std::string const& name, Expander const& expand, Generator const& generate)
        : describable { name }
        , expand { expand }
        , generate { generate }
      {}

      friend auto operator <<(std::ostream & os, syntax const& datum) -> std::ostream &
      {
        return os << magenta("#,(") << green("syntax ") << datum.name << magenta(")");
      }
    };

    using pair::pair;

    auto compile(object const& form) -> object;

    static auto core() -> object const&;

    static auto corename(std::string const&) -> object;

    auto define(object const&, object const& = undefined) -> void;

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> void
    {
      if constexpr (std::is_base_of_v<describable, T>)
      {
        return define(make_symbol(name), make<T>(name, std::forward<decltype(xs)>(xs)...));
      }
      else
      {
        return define(make_symbol(name), make<T>(std::forward<decltype(xs)>(xs)...));
      }
    }

    template <template <typename...> typename Deducer, typename... Ts>
    auto define(Ts&&... xs) -> decltype(auto)
    {
      return define<typename Deducer<Ts...>::type>(std::forward<decltype(xs)>(xs)...);
    }

    auto expand(object const& form, object const& bound_variables) const -> object;

    auto expand(object const& form, object const& bound_variables, typename syntactic_closure::renamer & rename) const -> object;

    auto generate(object const& form,
                  object const& bound_variables,
                  object const& continuation = list(make(instruction::secd_stop)),
                  bool tail = false) -> object;

    auto identify(object const& variable, object const& bound_variables) const -> object;

    auto identify(object const& variable, object const& bound_variables) -> object;

    auto sweep(let const& form,
               let const& sequence,
               let const& bound_variables,
               let const& current_environment,
               typename syntactic_closure::renamer & rename,
               let const& formals = unit,
               let const& reversed_binding_specs = unit) const -> std::tuple<object, object, object>;
  };
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_ENVIRONMENT_HPP
