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
    struct renamer
    {
      syntactic_closure const* enclosure;

      renamer * outer;

      bool transparent;

      let dictionary;

      explicit renamer(syntactic_closure const* enclosure, renamer * outer, bool transparent);

      auto count(let const& form) -> int;

      auto make_syntactic_closure(let const& form, int version = 0) -> object const&;

      auto unshadow(let const& formals, let const& bound_variables) -> object;

      auto memq(let const& form) const -> object;

      auto assq(let const& form) const -> object;

      auto rename(let const& form) -> object;

      auto operator ()(let const& form) -> object;

      auto operator ()(let const& formals, let const& bound_variables) -> object;
    };

    let environment, free_names, form;

    int version;

    explicit syntactic_closure(let const& environment,
                               let const& free_names,
                               let const& form,
                               int version = 0);

    auto expand(let const& bound_variables, renamer & outer) -> object;

    auto identify(let const& bound_variables) -> object;
  };

  auto operator ==(syntactic_closure const& x, syntactic_closure const& y) -> bool;

  auto operator <<(std::ostream & os, syntactic_closure const& datum) -> std::ostream &;
} // namespace meevax::kernel

#endif // INCLUDED_MEEVAX_KERNEL_SYNTACTIC_CLOSURE_HPP
