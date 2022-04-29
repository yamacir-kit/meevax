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

#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/identity.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  auto identity::load(const_reference e) -> reference
  {
    return const_cast<reference>(std::as_const(*this).load(e));
  }

  auto identity::symbol() const -> const_reference
  {
    assert(first.is<meevax::symbol>());
    return first;
  }

  auto operator <<(std::ostream & os, identity const& datum) -> std::ostream &
  {
    return os << magenta("#,(") << blue("identity ") << datum.symbol() << magenta(")");
  }

  auto absolute::is_bound() const -> bool
  {
    return not is_free();
  }

  auto absolute::is_free() const -> bool
  {
    return load().is<unbound>();
  }

  auto absolute::load(const_reference) const -> const_reference
  {
    return second;
  }

  auto absolute::load(const_reference) -> reference
  {
    return second;
  }

  auto absolute::make_load_instruction() const -> object
  {
    return make<instruction>(mnemonic::load_absolute);
  }

  auto absolute::make_store_instruction() const -> object
  {
    return make<instruction>(mnemonic::store_absolute);
  }

  auto relative::is_bound() const -> bool
  {
    return true;
  }

  auto relative::is_free() const -> bool
  {
    return false;
  }

  auto relative::load(const_reference e) const -> const_reference
  {
    return list_ref(list_ref(e, car(second)), cdr(second));
  }

  auto relative::make_load_instruction() const -> object
  {
    return make<instruction>(mnemonic::load_relative);
  }

  auto relative::make_store_instruction() const -> object
  {
    return make<instruction>(mnemonic::store_relative);
  }

  auto variadic::load(const_reference e) const -> const_reference
  {
    return list_tail(list_ref(e, car(second)), cdr(second));
  }

  auto variadic::make_load_instruction() const -> object
  {
    return make<instruction>(mnemonic::load_variadic);
  }

  auto variadic::make_store_instruction() const -> object
  {
    return make<instruction>(mnemonic::store_variadic);
  }
} // namespace kernel
} // namespace meevax
