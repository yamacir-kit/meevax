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

namespace meevax
{
inline namespace kernel
{
  struct library : public environment
  {
    let const declarations = unit;

    let export_specs = unit;

    let identifiers = unit;

    template <typename F, REQUIRES(std::is_invocable<F, library &>)>
    explicit library(F&& f)
    {
      std::invoke(std::forward<decltype(f)>(f), *this);
    }

    explicit library(const_reference);

    static auto boot() -> void;

    auto build() -> void;

    template <typename T, typename... Ts>
    auto define(std::string const& name, Ts&&... xs) -> void
    {
      environment::define<T>(name, std::forward<decltype(xs)>(xs)...);
      export_(name);
    }

    auto evaluate(const_reference) -> void;

    auto export_(const_reference) -> void;

    auto export_(std::string const&) -> void;

    auto resolve() -> const_reference;
  };

  auto operator <<(std::ostream &, library const&) -> std::ostream &;

  extern std::unordered_map<std::string, library> libraries;

  template <typename... Ts>
  auto define_library(std::string const& name, Ts&&... xs)
  {
    return libraries.emplace(name, std::forward<decltype(xs)>(xs)...);
  }
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
