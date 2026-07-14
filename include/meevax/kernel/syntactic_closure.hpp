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

#ifndef INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP
#define INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP

#include <meevax/kernel/identifier.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax::inline kernel
{
  struct syntactic_closure : public identifier
  {
    struct alpha
    {
      syntactic_closure const* enclosure;

      alpha * outer;

      let dictionary;

      explicit alpha(syntactic_closure const* enclosure, alpha * outer);

      auto convert(let const& form) -> object;

      auto convert_formals(let const& formals, let const& bound_variables) -> object;

      auto make_syntactic_closure(let const& form, int) -> object const&;
    };

    let syntactic_environment, free_names, form;

    int de_bruijn_level;

    explicit syntactic_closure(let const& syntactic_environment,
                               let const& free_names,
                               let const& form,
                               int = 0);

    auto expand(let const& bound_variables, alpha & outer) -> object;

    auto identify(let const& bound_variables) -> object;
  };

  auto operator ==(syntactic_closure const& x, syntactic_closure const& y) -> bool;

  auto operator <<(std::ostream & os, syntactic_closure const& datum) -> std::ostream &;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP
