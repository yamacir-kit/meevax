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
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  auto absolute::load() const -> object const&
  {
    if (second.is_also<absolute>()) // NOTE: Only the (export (rename ...)) form makes an identity whose value is identity.
    {
      assert(second.is<absolute>());
      return second.as<absolute>().load();
    }
    else
    {
      return second;
    }
  }

  auto absolute::load() -> object &
  {
    return const_cast<object &>(std::as_const(*this).load());
  }

  auto absolute::make_load_instruction() const -> object
  {
    return make(instruction::load_absolute);
  }

  auto absolute::make_store_instruction() const -> object
  {
    return make(instruction::store_absolute);
  }

  auto absolute::symbol() const -> object const&
  {
    assert(first.is_also<identifier>());
    return first;
  }

  auto operator <<(std::ostream & os, absolute const& datum) -> std::ostream &
  {
    return os << datum.symbol();
  }

  auto relative::load(object const& e) -> object &
  {
    return const_cast<object &>(std::as_const(*this).load(e));
  }

  auto relative::load(object const& e) const -> object const&
  {
    assert(first.is<std::uint32_t>());
    assert(second.is<std::uint32_t>());

    return e[first.as<std::uint32_t>()][second.as<std::uint32_t>()];
  }

  auto relative::make_load_instruction() const -> object
  {
    return make(instruction::load_relative);
  }

  auto relative::make_store_instruction() const -> object
  {
    return make(instruction::store_relative);
  }

  auto operator ==(relative const&, relative const&) -> bool
  {
    return false; // No viable comparison.
  }

  auto variadic::load(object const& e) -> object &
  {
    return const_cast<object &>(std::as_const(*this).load(e));
  }

  auto variadic::load(object const& e) const -> object const&
  {
    assert(first.is<std::uint32_t>());
    assert(second.is<std::uint32_t>());

    return tail(e[first.as<std::uint32_t>()], second.as<std::uint32_t>());
  }

  auto variadic::make_load_instruction() const -> object
  {
    return make(instruction::load_variadic);
  }

  auto variadic::make_store_instruction() const -> object
  {
    return make(instruction::store_variadic);
  }

  auto operator ==(variadic const&, variadic const&) -> bool
  {
    return false; // No viable comparison.
  }
} // namespace kernel
} // namespace meevax
