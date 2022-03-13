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

namespace meevax
{
inline namespace kernel
{
  struct notation : public virtual pair
  {
    using pair::pair;

    virtual auto mnemonic() const -> mnemonic = 0;
  };

  struct relative : public notation // de_bruijn_index
  {
    using notation::notation;

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

    virtual auto strip(const_reference e) const -> object
    {
      return list_ref(list_ref(e, m()), n());
    }
  };

  struct variadic : public relative // de_bruijn_index
  {
    using relative::relative;

    auto mnemonic() const -> meevax::mnemonic override
    {
      return mnemonic::load_variadic;
    }

    auto strip(const_reference e) const -> object override
    {
      return list_tail(list_ref(e, m()), n());
    }
  };
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_NOTATION_HPP
