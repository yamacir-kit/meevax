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

#ifndef INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
#define INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP

#include <meevax/kernel/environment.hpp>
#include <meevax/kernel/export_spec.hpp>

namespace meevax
{
inline namespace kernel
{
  struct library : public environment
  {
    let declarations = unit;

    let subset = unit;

    template <typename T, typename = void>
    struct is_library_declaration : public std::false_type
    {};

    template <typename T>
    struct is_library_declaration<T, std::void_t<decltype(std::declval<T>().resolve(std::declval<library &>()))>>
      : public std::true_type
    {};

    template <typename F, REQUIRES(std::is_invocable<F, library &>)>
    explicit library(F&& f)
    {
      std::invoke(std::forward<decltype(f)>(f), *this);
    }

    explicit library(const_reference);

    static auto boot() -> void;

    template <typename T, typename... Ts>
    auto declare(Ts&&... xs) -> decltype(auto)
    {
      if constexpr (is_library_declaration<std::decay_t<T>>::value)
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
      declare<export_spec>(read(name));
    }

    auto evaluate(const_reference) -> void;

    auto resolve() -> const_reference;
  };

  auto operator <<(std::ostream &, library const&) -> std::ostream &;

  /*
     NOTE: In order to improve the usability of the help procedure, it is
     desirable to sort by library name in lexicographical order.
  */
  extern std::map<std::string, library> libraries;

  template <typename... Ts>
  auto define_library(std::string const& name, Ts&&... xs)
  {
    return libraries.emplace(name, std::forward<decltype(xs)>(xs)...);
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
