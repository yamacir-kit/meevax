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

#ifndef INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
#define INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP

#include <map>

#include <meevax/kernel/interaction_environment.hpp>

namespace meevax::inline kernel
{
  struct library
  {
    environment evaluator;

    let declarations = unit;

    let export_specs = unit;

    template <typename F, typename = std::enable_if_t<std::is_invocable_v<F, library &>>>
    explicit library(F declare)
    {
      declare(*this);
    }

    explicit library(object const&);

    friend auto boot() -> void;

    auto define(std::string const& name, object const& x)
    {
      let const identifier = make_symbol(name);
      evaluator.define(identifier, x);
      export_specs = cons(identifier, export_specs);
    }

    auto evaluate(object const&) -> object;

    auto import_set() -> object;
  };

  auto operator <<(std::ostream &, library const&) -> std::ostream &;

  auto libraries() -> std::map<std::string, object> &;

  template <typename T>
  auto define(std::string const& name, auto&&... xs) -> decltype(auto)
  {
    if constexpr (std::is_same_v<T, library>)
    {
      return libraries().emplace(name, make<library>(std::forward<decltype(xs)>(xs)...));
    }
    else
    {
      return interaction_environment().as<environment>().define<T>(name, std::forward<decltype(xs)>(xs)...);
    }
  }
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_LIBRARY_HPP
