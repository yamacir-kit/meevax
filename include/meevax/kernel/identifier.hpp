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

#include <meevax/kernel/notation.hpp>

namespace meevax
{
inline namespace kernel
{
  struct identifier : public virtual pair
  {
    using pair::pair;

    virtual auto is_bound() const -> bool = 0;

    virtual auto is_free() const -> bool = 0;

    auto symbol() const -> const_reference;
  };

  auto operator <<(std::ostream &, identifier const&) -> std::ostream &;

  struct absolute : public identifier
                  , public notation
  {
    using identifier::identifier;

    auto mnemonic() const -> meevax::mnemonic override;

    auto binding() -> reference;

    auto binding() const -> const_reference;

    auto is_bound() const -> bool override;

    auto is_free() const -> bool override;
  };

  struct keyword : public absolute
  {
    using absolute::absolute;
  };

  auto notate(const_reference, const_reference) -> object;
} // namespace kernel
} // namespace meevax

#endif // INCLUDED_MEEVAX_KERNEL_IDENTIFIER_HPP
