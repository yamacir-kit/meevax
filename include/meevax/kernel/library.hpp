/*
   Copyright 2018-2023 Tatsuya Yamasaki.

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

#ifndef INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
#define INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/export_spec.hpp>
#include <meevax/kernel/interaction_environment.hpp>

namespace meevax
{
inline namespace kernel
{
  struct library : public environment
  {
    template <typename T, typename = void>
    struct is_declaration
      : public std::false_type
    {};

    template <typename T>
    struct is_declaration<T, std::void_t<decltype(std::declval<T>().resolve(std::declval<library &>()))>>
      : public std::true_type
    {};

    template <typename T>
    static constexpr auto is_declaration_v = is_declaration<T>::value;

    let declarations = unit;

    let subset = unit;

    template <typename F, REQUIRES(std::is_invocable<F, library &>)>
    explicit library(F f)
    {
      f(*this);
    }

    explicit library(object const&);

    friend auto boot() -> void;

    template <typename T, typename... Ts>
    auto declare(Ts&&... xs) -> decltype(auto)
    {
      if constexpr (is_declaration_v<T>)
      {
        return std::decay_t<T>(std::forward<decltype(xs)>(xs)...).resolve(*this);
      }
      else
      {
        return environment::declare<T>(std::forward<decltype(xs)>(xs)...);
      }
    }

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> void
    {
      environment::define<T>(name, std::forward<decltype(xs)>(xs)...);
      declare<export_spec>(input_string_port(name).read());
    }

    auto evaluate(object const&) -> void;

    auto resolve() -> object const&;
  };

  auto operator <<(std::ostream &, library const&) -> std::ostream &;

  /*
     In order to improve the usability of the help procedure, it is desirable
     to sort by library name in lexicographical order.
  */
  auto libraries() -> std::map<std::string, library> &;

  template <typename T, typename... Ts>
  auto define(std::string const& name, Ts&&... xs) -> decltype(auto)
  {
    if constexpr (std::is_same_v<T, library>)
    {
      return libraries().emplace(name, std::forward<decltype(xs)>(xs)...);
    }
    else
    {
      return interaction_environment().as<environment>().define<T>(name, std::forward<decltype(xs)>(xs)...);
    }
  }

  auto boot() -> void;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
