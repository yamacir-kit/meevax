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

#ifndef INCLUDED_MEEVAX_KERNEL_IDENTITY_HPP
#define INCLUDED_MEEVAX_KERNEL_IDENTITY_HPP

#include <meevax/kernel/ghost.hpp>
#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  struct identity : public virtual pair
  {
    using pair::pair;

    virtual auto is_bound() const -> bool = 0;

    virtual auto is_free() const -> bool = 0;

    virtual auto make_load_instruction() const -> object = 0;

    virtual auto make_store_instruction() const -> object = 0;

    virtual auto load(const_reference) const -> const_reference = 0;

    virtual auto load(const_reference e) -> reference
    {
      return const_cast<reference>(std::as_const(*this).load(e));
    }

    virtual auto symbol() const -> const_reference
    {
      assert(first.is<meevax::symbol>());
      return first;
    }

    friend auto operator <<(std::ostream & os, identity const& datum) -> std::ostream &
    {
      return os << magenta("#,(") << blue("identity ") << datum.symbol() << magenta(")");
    }
  };

  struct absolute : public identity
  {
    using identity::identity;

    auto is_bound() const -> bool override
    {
      return not is_free();
    }

    auto is_free() const -> bool override
    {
      return load().is<unbound>();
    }

    auto make_load_instruction() const -> object override
    {
      return make<instruction>(mnemonic::load_absolute);
    }

    auto make_store_instruction() const -> object override
    {
      return make<instruction>(mnemonic::store_absolute);
    }

    auto load(const_reference = unit) const -> const_reference override
    {
      return second;
    }

    auto load(const_reference = unit) -> reference override
    {
      return second;
    }
  };

  struct keyword : public absolute
  {
    using absolute::absolute;
  };

  struct relative : public identity // (<symbol> . <de Bruijn index>) = (<symbol> <integer> . <integer>)
  {
    using identity::identity;

    auto is_bound() const -> bool override
    {
      return true;
    }

    auto is_free() const -> bool override
    {
      return false;
    }

    auto make_load_instruction() const -> object override
    {
      return make<instruction>(mnemonic::load_relative);
    }

    auto make_store_instruction() const -> object override
    {
      return make<instruction>(mnemonic::store_relative);
    }

    auto load(const_reference e) const -> const_reference override
    {
      return list_ref(list_ref(e, car(second)), cdr(second));
    }
  };

  struct variadic : public relative // (<symbol> . <de Bruijn index>) = (<symbol> <integer> . <integer>)
  {
    using relative::relative;

    auto make_load_instruction() const -> object override
    {
      return make<instruction>(mnemonic::load_variadic);
    }

    auto make_store_instruction() const -> object override
    {
      return make<instruction>(mnemonic::store_variadic);
    }

    auto load(const_reference e) const -> const_reference override
    {
      return list_tail(list_ref(e, car(second)), cdr(second));
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_IDENTITY_HPP
