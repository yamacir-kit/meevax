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

#ifndef INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
#define INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP

#include <meevax/kernel/instruction.hpp>
#include <meevax/kernel/pair.hpp>

namespace meevax
{
inline namespace kernel
{
  struct identifier : public virtual pair
  {
    using pair::pair;

    virtual auto corresponding_mnemonic() const -> mnemonic = 0;

    virtual auto is_bound() const -> bool = 0;

    virtual auto is_free() const -> bool = 0;

    auto symbol() const -> const_reference;
  };

  auto operator <<(std::ostream &, identifier const&) -> std::ostream &;

  struct absolute : public identifier
  {
    using identifier::identifier;

    auto corresponding_mnemonic() const -> mnemonic override;

    auto binding() -> reference;

    auto binding() const -> const_reference;

    auto is_bound() const -> bool override;

    auto is_free() const -> bool override;
  };

  struct keyword : public absolute
  {
    using absolute::absolute;
  };

  struct relative : public identifier // de_bruijn_index
  {
    using identifier::identifier;

    auto corresponding_mnemonic() const -> mnemonic override;

    auto is_bound() const -> bool override;

    auto is_free() const -> bool override;

    virtual auto strip(const_reference) const -> object;
  };

  struct variadic : public relative // de_bruijn_index
  {
    using relative::relative;

    auto corresponding_mnemonic() const -> mnemonic override;

    auto strip(const_reference) const -> object override;
  };

  auto notate(const_reference, const_reference) -> object;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
