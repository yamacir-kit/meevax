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

#ifndef INCLUDED_MEEVAX_KERNEL_NOTATION_HPP
#define INCLUDED_MEEVAX_KERNEL_NOTATION_HPP

#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/list.hpp>
#include <meevax/kernel/pair.hpp>
#include <meevax/kernel/symbol.hpp>

namespace meevax
{
inline namespace kernel
{
  struct notation : public virtual pair
  {
    using pair::pair;

    virtual auto strip(const_reference) const -> const_reference = 0;

    virtual auto strip(const_reference e) -> reference
    {
      return const_cast<reference>(std::as_const(*this).strip(e));
    }

    virtual auto mnemonic() const -> mnemonic = 0;

    virtual auto symbol() const -> const_reference
    {
      assert(first.is<meevax::symbol>());
      return first;
    }
  };

  struct absolute : public notation
  {
    using notation::notation;

    auto strip(const_reference) const -> const_reference override
    {
      return second;
    }

    auto mnemonic() const -> meevax::mnemonic override
    {
      return mnemonic::load_absolute;
    }

    auto binding() -> reference
    {
      return second;
    }

    auto binding() const -> const_reference
    {
      return second;
    }

    auto is_bound() const -> bool
    {
      return not is_free();
    }

    auto is_free() const -> bool
    {
      // NOTE: See environment::generate_free_identifier
      return binding().is<absolute>() and std::addressof(binding().as<absolute>()) == this;
    }
  };

  struct keyword : public absolute
  {
    using absolute::absolute;
  };

  struct relative : public notation // de_bruijn_index
  {
    using notation::notation;

    auto strip(const_reference e) const -> const_reference override
    {
      return list_ref(list_ref(e, m()), n());
    }

    auto mnemonic() const -> meevax::mnemonic override
    {
      return mnemonic::load_relative;
    }

    auto m() const -> const_reference
    {
      return car(second);
    }

    auto n() const -> const_reference
    {
      return cdr(second);
    }
  };

  struct variadic : public relative // de_bruijn_index
  {
    using relative::relative;

    auto strip(const_reference e) const -> const_reference override
    {
      return list_tail(list_ref(e, m()), n());
    }

    auto mnemonic() const -> meevax::mnemonic override
    {
      return mnemonic::load_variadic;
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NOTATION_HPP
